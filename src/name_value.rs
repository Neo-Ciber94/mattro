use crate::{lit_to_string, display_lit};
use syn::{Lit};
use std::str::FromStr;
use std::fmt::{Display, Formatter, Write};
use std::hash::{Hash, Hasher};

/// Represents an attribute name-value: `name="value"`.
#[derive(Debug, Clone)]
pub struct NameValue {
    pub name: String,
    pub value: Value,
}

impl Display for NameValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}={}", self.name, self.value)
    }
}

impl Hash for NameValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(self.name.as_bytes());
    }
}

impl Eq for NameValue {}

impl PartialEq for NameValue {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

/// Represents a value for a `name-value` attribute.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    /// A literal value: `#[attribute(name="value")]`.
    Literal(Lit),
    /// An array of literal values: `#[attribute(name=1,2,3,4)]`.
    Array(Vec<Lit>),
}

impl Value {
    /// Returns `true` if this value is a literal.
    pub fn is_literal(&self) -> bool {
        matches!(self, Value::Literal(_))
    }

    /// Returns `true` if this value is an array.
    pub fn is_array(&self) -> bool {
        matches!(self, Value::Array(_))
    }

    /// Returns `true` if this value is a `string` or `byte string` literal value.
    pub fn is_string(&self) -> bool {
        match self {
            Value::Literal(lit) => matches!(lit, Lit::Str(_) | Lit::ByteStr(_)),
            _ => false,
        }
    }

    /// Returns `true` if this value is a `char` literal value.
    pub fn is_char(&self) -> bool {
        matches!(self, Value::Literal(Lit::Char(_)))
    }

    /// Returns `true` if this value is a `bool` literal value.
    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Literal(Lit::Bool(_)))
    }

    /// Returns `true` if this value is an `integer` or `byte` literal value.
    pub fn is_integer(&self) -> bool {
        match self {
            Value::Literal(lit) => matches!(lit, Lit::Int(_) | Lit::Byte(_)),
            _ => false,
        }
    }

    /// Returns `true` if this value is a `float` literal value.
    pub fn is_float(&self) -> bool {
        matches!(self, Value::Literal(Lit::Float(_)))
    }

    /// Returns `true` if this value is a numeric literal (integer or float).
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// Returns the `String` representation of this value or `None` if is not a string literal.
    pub fn to_string_literal(&self) -> Option<String> {
        if let Value::Literal(lit) = self {
            return match lit {
                Lit::Str(x) => Some(x.value()),
                Lit::ByteStr(x) => unsafe { Some(String::from_utf8_unchecked(x.value())) },
                _ => None,
            };
        }

        None
    }

    /// Returns the `char` representation of this value or `None` if is not a char literal.
    pub fn to_char_literal(&self) -> Option<char> {
        if let Value::Literal(lit) = self {
            return match lit {
                Lit::Char(x) => Some(x.value()),
                _ => None,
            };
        }

        None
    }

    /// Returns the `bool` representation of this value or `None` if is not a bool literal.
    pub fn to_bool_literal(&self) -> Option<bool> {
        if let Value::Literal(lit) = self {
            return match lit {
                Lit::Bool(x) => Some(x.value),
                _ => None,
            };
        }

        None
    }

    /// Returns the `byte` representation of this value or `None` if is not a byte literal.
    pub fn to_byte_literal(&self) -> Option<u8> {
        if let Value::Literal(lit) = self {
            return match lit {
                Lit::Byte(x) => Some(x.value()),
                _ => None,
            };
        }

        None
    }

    /// Converts this value into a integer or `None` if is not an integer literal.
    pub fn to_integer_literal<N>(&self) -> Option<N>
        where
            N: FromStr,
            N::Err: Display,
    {
        match self {
            Value::Literal(lit) => match lit {
                Lit::Byte(n) => {
                    let s = n.value().to_string();
                    N::from_str(s.as_str()).ok()
                }
                Lit::Int(n) => n.base10_parse().ok(),
                _ => None,
            },
            _ => None,
        }
    }

    /// Converts this value into a float or `None` if is not a float literal.
    pub fn to_float_literal<N>(&self) -> Option<N>
        where
            N: FromStr,
            N::Err: Display,
    {
        match self {
            Value::Literal(Lit::Float(n)) => n.base10_parse().ok(),
            _ => None,
        }
    }

    /// Returns a reference to this value literal.
    pub fn as_literal(&self) -> Option<&Lit> {
        match self {
            Value::Literal(x) => Some(x),
            _ => None,
        }
    }

    /// Returns a reference to this value array of literals.
    pub fn as_array(&self) -> Option<&[Lit]> {
        match self {
            Value::Array(x) => Some(x.as_slice()),
            _ => None,
        }
    }

    /// Parses this value into the given type.
    ///
    /// # Returns None
    /// - `None` if the value is not a literal.
    /// - `None` if the parse fails.
    pub fn parse_literal<T: FromStr>(&self) -> Option<T> {
        match self {
            Value::Literal(x) => {
                let value = lit_to_string(x);
                T::from_str(&value).ok()
            }
            _ => None,
        }
    }

    /// Parses this value into a `Vec<T>`
    ///
    /// # Returns None
    /// - `None` if the value is not an array literal.
    /// - `None` if the parse fails.
    pub fn parse_array<T: FromStr>(&self) -> Option<Vec<T>> {
        match self {
            Value::Array(array) => {
                let mut ret = Vec::new();
                for arg in array {
                    let value = lit_to_string(arg);
                    let n = T::from_str(&value).ok()?;
                    ret.push(n);
                }
                Some(ret)
            }
            _ => None,
        }
    }

    /// Writes a `String` representation of this value using the given `Write`.
    ///
    /// # Arguments
    /// - `formatter` : The formatter used to write the data.
    /// - `use_array_brackets` : Wraps the arrays using brackets: `[1,2,3]`.
    pub fn display<W: Write>(
        &self,
        formatter: &mut W,
        use_array_brackets: bool,
    ) -> std::fmt::Result {
        match self {
            Value::Literal(lit) => display_lit(formatter, lit),
            Value::Array(array) => {
                let result = array
                    .iter()
                    .map(|s| lit_to_string(s))
                    .collect::<Vec<String>>();

                if use_array_brackets {
                    write!(formatter, "[{}]", result.join(", "))
                } else {
                    write!(formatter, "{}", result.join(", "))
                }
            }
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.display(f, true)
    }
}
