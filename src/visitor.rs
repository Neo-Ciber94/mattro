use crate::{Value, NameValue, MetaItem, MacroAttribute};
use syn::{NestedMeta, MetaNameValue, MetaList, Meta, AttributeArgs, Lit, Path};
use std::iter::Peekable;

/// Used for parse a attribute args.
pub struct AttributeArgsVisitor;

impl AttributeArgsVisitor {
    pub fn visit(attribute_args: AttributeArgs) -> Vec<MetaItem> {
        let mut meta_items = Vec::new();
        let mut iter = attribute_args.iter().peekable();

        while let Some(next) = iter.next() {
            match next {
                NestedMeta::Lit(lit) => AttributeArgsVisitor::visit_lit(&mut meta_items, lit),
                NestedMeta::Meta(meta) => {
                    AttributeArgsVisitor::visit_meta(&mut iter, &mut meta_items, meta)
                }
            }
        }

        meta_items
    }

    fn visit_lit(ret: &mut Vec<MetaItem>, lit: &Lit) {
        ret.push(MetaItem::Literal(lit.clone()))
    }

    fn visit_meta<'a, I>(iter: &mut Peekable<I>, ret: &mut Vec<MetaItem>, meta: &Meta)
        where
            I: Iterator<Item = &'a NestedMeta>,
    {
        match meta {
            Meta::Path(path) => AttributeArgsVisitor::visit_path(ret, path),
            Meta::List(list) => AttributeArgsVisitor::visit_list(ret, list),
            Meta::NameValue(name_value) => {
                AttributeArgsVisitor::visit_name_value(iter, ret, name_value)
            }
        }
    }

    fn visit_path(ret: &mut Vec<MetaItem>, path: &Path) {
        let name = join_path_to_string(path);
        ret.push(MetaItem::Path(name))
    }

    fn visit_list(ret: &mut Vec<MetaItem>, list: &MetaList) {
        let path = join_path_to_string(&list.path);
        let mut values = Vec::new();
        let mut iter = list.nested.iter().peekable();

        while let Some(next) = iter.next() {
            match next {
                NestedMeta::Lit(lit) => AttributeArgsVisitor::visit_lit(&mut values, lit),
                NestedMeta::Meta(meta) => {
                    AttributeArgsVisitor::visit_meta(&mut iter, &mut values, meta)
                }
            }
        }

        ret.push(MetaItem::Nested(MacroAttribute {
            path,
            args: values,
            style: None,
        }));
    }

    fn visit_name_value<'a, I>(
        iter: &mut Peekable<I>,
        ret: &mut Vec<MetaItem>,
        name_value: &MetaNameValue,
    ) where
        I: Iterator<Item = &'a NestedMeta>,
    {
        let key = join_path_to_string(&name_value.path);
        let mut values = Vec::new();
        values.push(name_value.lit.clone());

        while let Some(NestedMeta::Lit(lit)) = iter.peek() {
            values.push(lit.clone());
            iter.next();
        }

        debug_assert!(!values.is_empty());

        match values.len() {
            1 => {
                let value = Value::Literal(values.remove(0));
                ret.push(MetaItem::NameValue(NameValue { name: key, value }))
            }
            _ => ret.push(MetaItem::NameValue(NameValue {
                name: key,
                value: Value::Array(values),
            })),
        }
    }
}

pub fn join_path_to_string(path: &Path) -> String {
    path.segments
        .iter()
        .map(|s| s.ident.to_string())
        .collect::<Vec<String>>()
        .join("::")
}
