use std::convert::TryFrom;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Index;

use linked_hash_set::LinkedHashSet;
use syn::{AttrStyle, Attribute, AttributeArgs};

use crate::{MacroAttribute, MetaItem, NameValue, Value, lit_to_string};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NameValueAttribute {
    path: String,
    args: LinkedHashSet<NameValue>,
    style: AttrStyle,
}

impl NameValueAttribute {
    pub fn empty(path: String, style: AttrStyle) -> Self {
        NameValueAttribute {
            path,
            args: Default::default(),
            style,
        }
    }

    pub fn from_args(path: String, style: AttrStyle, args: LinkedHashSet<NameValue>) -> Self {
        NameValueAttribute { path, args, style }
    }

    pub fn new(
        path: &str,
        meta_items: Vec<MetaItem>,
        style: AttrStyle,
    ) -> Result<Self, NameValueError> {
        let mut args = LinkedHashSet::new();

        for meta_item in meta_items.into_iter() {
            let name_value = meta_item
                .as_name_value()
                .cloned()
                .ok_or_else(|| NameValueError::InvalidValue(meta_item.clone()))?;

            if args.contains(&name_value) {
                return Err(NameValueError::DuplicatedKey(name_value.name));
            } else {
                args.insert(name_value);
            }
        }

        Ok(NameValueAttribute::from_args(path.to_owned(), style, args))
    }

    pub fn from_attribute_args(
        path: &str,
        attribute_args: AttributeArgs,
        style: AttrStyle,
    ) -> Result<Self, NameValueError> {
        MacroAttribute::from_attribute_args(path, attribute_args, style).into_name_values()
    }

    pub fn path(&self) -> &str {
        self.path.as_str()
    }

    pub fn style(&self) -> &AttrStyle {
        &self.style
    }

    pub fn len(&self) -> usize {
        self.args.len()
    }

    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.args.iter().find(|v| v.name == name).map(|v| &v.value)
    }

    pub fn contains(&self, name: &str) -> bool {
        self.args.iter().any(|v| v.name == name)
    }

    pub fn iter(&self) -> Iter<'_> {
        Iter {
            iter: self.args.iter(),
        }
    }
}

impl<'a> Index<&'a str> for NameValueAttribute {
    type Output = Value;

    fn index(&self, index: &'a str) -> &Self::Output {
        self.get(index).expect("invalid name")
    }
}

#[derive(Debug, Clone)]
pub struct Iter<'a> {
    iter: linked_hash_set::Iter<'a, NameValue>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = (&'a String, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|v| (&v.name, &v.value))
    }
}

impl<'a> IntoIterator for &'a NameValueAttribute {
    type Item = (&'a String, &'a Value);
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct IntoIter {
    iter: linked_hash_set::IntoIter<NameValue>,
}

impl Iterator for IntoIter {
    type Item = (String, Value);

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some(NameValue { name, value }) => Some((name, value)),
            None => None,
        }
    }
}

impl IntoIterator for NameValueAttribute {
    type Item = (String, Value);
    type IntoIter = IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            iter: self.args.into_iter(),
        }
    }
}

impl TryFrom<Attribute> for NameValueAttribute {
    type Error = NameValueError;

    fn try_from(value: Attribute) -> Result<Self, Self::Error> {
        match MacroAttribute::new(value) {
            Ok(attr) => attr.into_name_values(),
            Err(_) => Err(NameValueError::InvalidAttribute),
        }
    }
}

impl Display for NameValueAttribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let style = if matches!(self.style, AttrStyle::Outer) {
            "#".to_owned()
        } else {
            "#!".to_owned()
        };

        let meta = self
            .args
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<String>>();

        if meta.is_empty() {
            write!(f, "{}[{}]", style, self.path())
        } else {
            write!(f, "{}[{}({})]", style, self.path(), meta.join(", "))
        }
    }
}

/// Represents an error when parsing a name-value attribute.
pub enum NameValueError {
    /// The name-value contains a not name-value attribute.
    InvalidValue(MetaItem),
    /// The name-value contains a duplicated name key.
    DuplicatedKey(String),
    /// The name-value attribute is in a invalid format.
    InvalidAttribute,
}

impl Debug for NameValueError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NameValueError::InvalidValue(x) => {
                write!(f, "`{}` is not a name-value", meta_item_to_string(&x))
            }
            NameValueError::DuplicatedKey(x) => write!(f, "duplicated key: `{}`", x),
            _ => panic!(),
        }
    }
}

pub fn meta_item_to_string(data: &MetaItem) -> String {
    match data {
        MetaItem::Path(path) => path.to_owned(),
        MetaItem::Literal(lit) => lit_to_string(lit),
        MetaItem::NameValue(data) => match &data.value {
            Value::Literal(x) => format!("{} = {}", data.name, lit_to_string(x)),
            Value::Array(x) => {
                let s = x.iter().map(lit_to_string).collect::<Vec<String>>();
                format!("{} = {:?}", data.name, s)
            }
        },
        MetaItem::Nested(data) => {
            if data.len() > 0 {
                data.path.clone()
            } else {
                format!("{}(...)", data.clone().path)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::MacroAttribute;
    use proc_macro2::TokenStream;
    use quote::*;
    use syn::parse_quote::ParseQuote;
    use syn::Attribute;

    fn parse_attr(tokens: TokenStream) -> Attribute {
        syn::parse::Parser::parse2(Attribute::parse, tokens).expect("invalid attribute")
    }

    #[test]
    fn into_name_value_test() {
        let tokens = quote! { #[person(name="Kaori", age=20)] };
        let raw_attr = MacroAttribute::new(parse_attr(tokens)).unwrap();
        assert!(raw_attr.into_name_values().is_ok());
    }

    #[test]
    fn into_name_value_error_test() {
        let tokens = quote! { #[person(name="Kaori", age=20, job(salary=200.0))] };
        let raw_attr = MacroAttribute::new(parse_attr(tokens)).unwrap();
        assert!(raw_attr.into_name_values().is_err());
    }

    #[test]
    fn into_name_value_duplicate_name_test() {
        let tokens = quote! { #[person(name="Kaori", age=20, age=21)] };
        let raw_attr = MacroAttribute::new(parse_attr(tokens)).unwrap();
        assert!(raw_attr.into_name_values().is_err());
    }

    #[test]
    fn new_name_value_attr_test() {
        let tokens = quote! { #[person(name="Kaori", age=20, fav_numbers=2,4,7)] };
        let raw_attr = MacroAttribute::new(parse_attr(tokens)).unwrap();
        let attr = raw_attr.into_name_values().unwrap();

        assert_eq!(attr.path, "person".to_owned());
        assert_eq!(attr.len(), 3);

        assert!(attr["name"].is_string());
        assert!(attr["age"].is_integer());
        assert!(attr["fav_numbers"].is_array());

        assert_eq!(attr["name"].to_string_literal(), Some("Kaori".to_owned()));
        assert_eq!(attr["age"].parse_literal::<u32>(), Some(20));
        assert_eq!(attr["fav_numbers"].parse_array(), Some(vec!(2, 4, 7)));
    }

    #[test]
    fn contains_name_test() {
        let tokens = quote! { #[person(name="Kaori", age=20, fav_numbers=2,4,7)] };
        let raw_attr = MacroAttribute::new(parse_attr(tokens)).unwrap();
        let attr = raw_attr.into_name_values().unwrap();

        assert!(attr.contains("name"));
        assert!(attr.contains("age"));
        assert!(attr.contains("fav_numbers"));
    }

    #[test]
    fn get_test() {
        let tokens = quote! { #[person(name="Kaori", age=20, fav_numbers=2,4,7)] };
        let raw_attr = MacroAttribute::new(parse_attr(tokens)).unwrap();
        let attr = raw_attr.into_name_values().unwrap();

        assert_eq!(
            attr.get("name").unwrap().to_string_literal(),
            Some("Kaori".to_owned())
        );
        assert_eq!(attr.get("age").unwrap().parse_literal::<u32>(), Some(20));
        assert_eq!(
            attr.get("fav_numbers").unwrap().parse_array(),
            Some(vec!(2, 4, 7))
        );
    }

    #[test]
    fn value_variant_check_test() {
        let tokens = quote! {
            #[values(
                str="hello",
                bytestr=b"world",
                byte=b'a',
                number=20,
                float=0.5,
                boolean=true,
                character='z',
                array=1,2,3
            )]
        };

        let raw_attr = MacroAttribute::new(parse_attr(tokens)).unwrap();
        let attr = raw_attr.into_name_values().unwrap();

        assert!(attr["str"].is_literal());
        assert!(attr["bytestr"].is_literal());
        assert!(attr["byte"].is_literal());
        assert!(attr["number"].is_literal());
        assert!(attr["float"].is_literal());
        assert!(attr["boolean"].is_literal());
        assert!(attr["character"].is_literal());
        assert!(attr["array"].is_array());
    }

    #[test]
    fn value_as_type_test() {
        let tokens = quote! {
            #[values(
                str="hello",
                bytestr=b"world",
                byte=b'a',
                number=20,
                float=0.5,
                boolean=true,
                character='z',
                array=1,2,3
            )]
        };

        let raw_attr = MacroAttribute::new(parse_attr(tokens)).unwrap();
        let attr = raw_attr.into_name_values().unwrap();

        assert!(attr["str"].as_literal().is_some());
        assert!(attr["array"].as_array().is_some());

        assert_eq!(attr["str"].to_string_literal(), Some("hello".to_string()));
        assert_eq!(
            attr["bytestr"].to_string_literal(),
            Some("world".to_string())
        );
        assert_eq!(attr["byte"].to_byte_literal(), Some(b'a'));
        assert_eq!(attr["boolean"].to_bool_literal(), Some(true));
        assert_eq!(attr["character"].to_char_literal(), Some('z'));
    }

    #[test]
    fn value_parse_test() {
        let tokens = quote! {
            #[values(
                str="hello",
                bytestr=b"world",
                byte=b'a',
                number=20,
                float=0.5,
                boolean=true,
                character='z',
                array=1,2,3
            )]
        };

        let raw_attr = MacroAttribute::new(parse_attr(tokens)).unwrap();
        let attr = raw_attr.into_name_values().unwrap();

        assert_eq!(
            attr["str"].parse_literal::<String>(),
            Some("hello".to_string())
        );
        assert_eq!(
            attr["bytestr"].parse_literal::<String>(),
            Some("world".to_string())
        );
        assert_eq!(attr["byte"].parse_literal::<u8>(), Some(b'a'));
        assert_eq!(attr["number"].parse_literal::<u32>(), Some(20));
        assert_eq!(attr["float"].parse_literal::<f64>(), Some(0.5));
        assert_eq!(attr["boolean"].parse_literal::<bool>(), Some(true));
        assert_eq!(attr["character"].parse_literal::<char>(), Some('z'));
        assert_eq!(attr["array"].parse_array::<usize>(), Some(vec!(1, 2, 3)));
    }

    #[test]
    fn to_type_test() {
        let tokens = quote! {
            #[values(
                str="hello",
                bytestr=b"world",
                byte=b'a',
                number=20,
                float=0.5,
                boolean=true,
                character='z'
            )]
        };

        let raw_attr = MacroAttribute::new(parse_attr(tokens)).unwrap();
        let attr = raw_attr.into_name_values().unwrap();

        assert_eq!(attr["str"].to_string_literal(), Some("hello".to_string()));
        assert_eq!(
            attr["bytestr"].to_string_literal(),
            Some("world".to_string())
        );
        assert_eq!(attr["byte"].to_byte_literal(), Some(b'a'));
        assert_eq!(attr["number"].to_integer_literal::<u32>(), Some(20));
        assert_eq!(attr["byte"].to_integer_literal::<u8>(), Some(b'a'));
        assert_eq!(attr["float"].to_float_literal::<f64>(), Some(0.5));
        assert_eq!(attr["boolean"].to_bool_literal(), Some(true));
        assert_eq!(attr["character"].to_char_literal(), Some('z'));

        assert_eq!(attr["str"].to_char_literal(), None);
        assert_eq!(attr["bytestr"].to_char_literal(), None);
        assert_eq!(attr["byte"].to_string_literal(), None);
        assert_eq!(attr["number"].to_char_literal(), None);
        assert_eq!(attr["byte"].to_bool_literal(), None);
        assert_eq!(attr["float"].to_integer_literal::<u32>(), None);
        assert_eq!(attr["boolean"].to_float_literal::<f64>(), None);
        assert_eq!(attr["character"].to_bool_literal(), None);
    }
}
