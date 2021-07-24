use std::fmt::{Display, Formatter, Write};
use std::ops::Index;
use std::slice::SliceIndex;

use syn::{
    AttrStyle, Attribute, AttributeArgs, Lit
};

use crate::{NameValue, NameValueAttribute, NameValueError};
use crate::visitor::{AttributeArgsVisitor, join_path_to_string};

/// Represents a macro attribute and its arguments like:
///
/// `#[attribute(key="value")]`
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MacroAttribute {
    pub(crate) path: String,
    pub(crate) args: Vec<MetaItem>,
    pub(crate) style: Option<AttrStyle>,
}

impl MacroAttribute {
    /// Constructs a new `MacroAttribute` from an `Attribute`.
    pub fn new(attribute: Attribute) -> syn::Result<Self> {
        let path = join_path_to_string(&attribute.path);
        let attr_args = get_attribute_args(&attribute)?;
        let args = AttributeArgsVisitor::visit(attr_args);
        let style = Some(attribute.style);

        Ok(MacroAttribute { path, args, style })
    }

    /// Constructs a `MacroAttribute` from an `AttributeArgs`.
    pub fn from_attribute_args(
        path: &str,
        attribute_args: AttributeArgs,
        style: AttrStyle,
    ) -> Self {
        let args = AttributeArgsVisitor::visit(attribute_args);
        MacroAttribute {
            path: path.to_string(),
            args,
            style: Some(style),
        }
    }

    /// Returns the `path` of an attribute.
    ///
    /// For `#[attribute(name="value")]` the path is `"attribute"`.
    pub fn path(&self) -> &str {
        self.path.as_str()
    }

    /// Returns the style of this attribute: outer or inner.
    pub fn style(&self) -> Option<&AttrStyle> {
        self.style.as_ref()
    }

    /// Returns the arguments of the attribute.
    ///
    /// For `#[attribute(name="value", number=10)]` the arguments are `"name=value"` and `"number=10"`.
    pub fn args(&self) -> &[MetaItem] {
        self.args.as_slice()
    }

    /// Returns the number of arguments in this attribute.
    pub fn len(&self) -> usize {
        self.args.len()
    }

    /// Returns `true` is this macro attribute have no arguments.
    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    /// Returns the `MetaItem` in the given index, or `None` if not found.
    pub fn get(&self, index: usize) -> Option<&MetaItem> {
        self.args.get(index)
    }

    /// Returns an iterator over the arguments in this attribute.
    pub fn iter(&self) -> impl Iterator<Item = &MetaItem> {
        self.args.iter()
    }

    /// Converts this macro attribute into a name-value attribute.
    pub fn into_name_values(self) -> Result<NameValueAttribute, NameValueError> {
        NameValueAttribute::new(self.path.as_str(), self.args, self.style.unwrap())
    }

    /// Converts this macro attribute into a list of its arguments.
    pub fn into_inner(self) -> Vec<MetaItem> {
        self.args
    }
}

impl<I: SliceIndex<[MetaItem]>> Index<I> for MacroAttribute {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        self.args.index(index)
    }
}

impl<'a> IntoIterator for &'a MacroAttribute {
    type Item = &'a MetaItem;
    type IntoIter = std::slice::Iter<'a, MetaItem>;

    fn into_iter(self) -> Self::IntoIter {
        self.args.iter()
    }
}

impl IntoIterator for MacroAttribute {
    type Item = MetaItem;
    type IntoIter = std::vec::IntoIter<MetaItem>;

    fn into_iter(self) -> Self::IntoIter {
        self.args.into_iter()
    }
}

impl Display for MacroAttribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            attribute_to_string(self.style.as_ref(), self.path(), self.args.as_slice())
        )
    }
}

/// Represents the data in an attribute.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum MetaItem {
    /// A path like: `#[attribute]`
    Path(String),
    /// A literal like: `#[attribute("hello world")]`
    Literal(Lit),
    /// A key-value like: `#[attribute(key="value")]` or `#[attribute(array=1,2,3,4)]`
    NameValue(NameValue),
    /// Nested data like: `#[attribute(inner("hello"))]`
    Nested(MacroAttribute),
}

impl MetaItem {
    /// Returns `true` if this meta item is a path like: `#[attribute]`.
    pub fn is_path(&self) -> bool {
        matches!(self, MetaItem::Path(_))
    }

    /// Returns `true` if this meta item is a literal like: `#[attribute("hola mundo")]`.
    pub fn is_literal(&self) -> bool {
        matches!(self, MetaItem::Literal(_))
    }

    /// Returns `true` if this meta item is a name-value pair like: `#[attribute(name="value")]`.
    pub fn is_name_value(&self) -> bool {
        matches!(self, MetaItem::NameValue(_))
    }

    /// Returns `true` if this meta item is a nested attribute like: `#[attribute(inner("hello"))]`.
    pub fn is_nested(&self) -> bool {
        matches!(self, MetaItem::Nested(_))
    }

    /// Converts this meta item into a `String` or `None` if is not a path.
    pub fn into_path(self) -> Option<String> {
        match self {
            MetaItem::Path(x) => Some(x),
            _ => None,
        }
    }

    /// Converts this meta item into a `Lit` or `None` if is not a literal.
    pub fn into_literal(self) -> Option<Lit> {
        match self {
            MetaItem::Literal(x) => Some(x),
            _ => None,
        }
    }

    /// Converts this meta item into a `NameValue` or `None` if is not a name-value pair.
    pub fn into_name_value(self) -> Option<NameValue> {
        match self {
            MetaItem::NameValue(x) => Some(x),
            _ => None,
        }
    }

    /// Converts this meta item into a its inner `MacroAttribute` or `None` if is not a nested attribute.
    pub fn into_nested(self) -> Option<MacroAttribute> {
        match self {
            MetaItem::Nested(x) => Some(x),
            _ => None,
        }
    }

    /// Returns a reference to this meta item as a `&str` or `None` if is not a path.
    pub fn as_path(&self) -> Option<&str> {
        match self {
            MetaItem::Path(x) => Some(x.as_str()),
            _ => None,
        }
    }

    /// Returns a reference to this meta item as a `Lit` or `None` if is not a literal.
    pub fn as_literal(&self) -> Option<&Lit> {
        match self {
            MetaItem::Literal(x) => Some(x),
            _ => None,
        }
    }

    /// Returns a reference to this meta item as a `NameValue` or `None` if is not a name-value pair.
    pub fn as_name_value(&self) -> Option<&NameValue> {
        match self {
            MetaItem::NameValue(x) => Some(x),
            _ => None,
        }
    }

    /// Returns a reference to this meta item as a nested macro attribute or `None` if is not a macro attribute.
    pub fn as_nested(&self) -> Option<&MacroAttribute> {
        match self {
            MetaItem::Nested(x) => Some(x),
            _ => None,
        }
    }
}

impl Display for MetaItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MetaItem::Path(s) => write!(f, "{}", s),
            MetaItem::Literal(lit) => display_lit(f, lit),
            MetaItem::NameValue(name_value) => write!(f, "{}", name_value),
            MetaItem::Nested(nested) => {
                assert!(
                    nested.style.is_none(),
                    "Nested macro attributes cannot have an style"
                );
                write!(
                    f,
                    "{}",
                    attribute_to_string(None, nested.path(), nested.args.as_slice())
                )
            }
        }
    }
}

fn get_attribute_args(attr: &Attribute) -> syn::Result<AttributeArgs> {
    // Get the token tree of the attribute.
    let mut token_tree = attr.tokens.clone().into_iter();

    // Skips the first parentheses of the attribute `(` and `)`
    if let Some(proc_macro2::TokenTree::Group(group)) = token_tree.next() {
        use syn::parse_macro_input::ParseMacroInput;
        let tokens = group.stream();
        syn::parse::Parser::parse2(AttributeArgs::parse, tokens)
    } else {
        // If the attribute is just #[attribute()] or #[attribute]
        Ok(AttributeArgs::new())
    }
}

#[doc(hidden)]
pub fn lit_to_string(lit: &Lit) -> String {
    match lit {
        Lit::Str(s) => s.value(),
        Lit::ByteStr(s) => unsafe { String::from_utf8_unchecked(s.value()) },
        Lit::Byte(s) => s.value().to_string(),
        Lit::Char(s) => s.value().to_string(),
        Lit::Int(s) => s.to_string(),
        Lit::Float(s) => s.to_string(),
        Lit::Bool(s) => s.value.to_string(),
        Lit::Verbatim(s) => s.to_string(),
    }
}

#[doc(hidden)]
pub fn display_lit<W: Write>(f: &mut W, lit: &Lit) -> std::fmt::Result {
    match lit {
        Lit::Str(s) => write!(f, "\"{}\"{}", s.value(), s.suffix()),
        Lit::ByteStr(s) => write!(
            f,
            "b{:?}{}",
            unsafe { String::from_utf8_unchecked(s.value()) },
            s.suffix()
        ),
        Lit::Byte(s) => write!(
            f,
            "b\'{}\'{}",
            std::char::from_u32(s.value() as u32).unwrap(),
            s.suffix()
        ),
        Lit::Char(s) => write!(f, "\'{}\'{}", s.value(), s.suffix()),
        Lit::Int(s) => write!(f, "{}{}", s.base10_digits(), s.suffix()),
        Lit::Float(s) => write!(f, "{}{}", s.base10_digits(), s.suffix()),
        Lit::Bool(s) => write!(f, "{}", s.value),
        Lit::Verbatim(s) => write!(f, "{}", s),
    }
}

#[doc(hidden)]
pub fn attribute_to_string<'a, I>(style: Option<&AttrStyle>, path: &str, meta_items: I) -> String
where
    I: IntoIterator<Item = &'a MetaItem>,
{
    let meta = meta_items
        .into_iter()
        .map(|s| s.to_string())
        .collect::<Vec<String>>();

    if let Some(style) = style {
        let style = if matches!(style, AttrStyle::Outer) {
            "#".to_owned()
        } else {
            "#!".to_owned()
        };

        if meta.is_empty() {
            format!("{}[{}]", style, path)
        } else {
            format!("{}[{}({})]", style, path, meta.join(", "))
        }
    } else {
        format!("{}({})", path, meta.join(", "))
    }
}

#[cfg(test)]
mod tests {
    use proc_macro2::TokenStream;
    use quote::*;
    use syn::parse_quote::ParseQuote;

    use super::*;

    fn parse_attr(tokens: TokenStream) -> Attribute {
        syn::parse::Parser::parse2(Attribute::parse, tokens).expect("invalid attribute")
    }

    #[test]
    fn new_macro_attr_test() {
        let tokens = quote! { #[person(name="Kaori", age=20, job(salary=200.0))] };
        let attr = MacroAttribute::new(parse_attr(tokens)).unwrap();

        assert_eq!(attr.path, "person".to_owned());
        assert_eq!(attr.len(), 3);
        assert!(attr[0].is_name_value());
        assert!(attr[1].is_name_value());
        assert!(attr[2].is_nested());
    }

    #[test]
    fn path_and_nested_test() {
        let tokens = quote! { #[attribute(path, nested())] };
        let attr = MacroAttribute::new(parse_attr(tokens)).unwrap();

        assert_eq!(attr.path, "attribute".to_owned());
        assert_eq!(attr.len(), 2);
        assert!(attr[0].is_path());
        assert!(attr[1].is_nested());
    }

    #[test]
    fn to_string_test() {
        let tokens = quote! {
            #[attribute(
                path,
                nested(name="Alan"),
                empty(),
                string="string",
                byte_str= b"byte_string",
                int=100usize,
                float=0.5,
                byte=b'a',
                boolean=true,
                character='z',
            )]
        };
        let attr = MacroAttribute::new(parse_attr(tokens)).unwrap();

        assert_eq!(
            attr.to_string().as_str(),
            "#[attribute(\
                path, \
                nested(name=\"Alan\"), \
                empty(), \
                string=\"string\", \
                byte_str=b\"byte_string\", \
                int=100usize, \
                float=0.5, \
                byte=b'a', \
                boolean=true, \
                character='z'\
            )]"
        )
    }

    #[test]
    fn invalid_attribute_test() {
        let tokens = quote! { #[attribute(let x = 10)] };
        let attr = parse_attr(tokens);
        let attribute = MacroAttribute::new(attr);
        assert!(attribute.is_err());
    }
}
