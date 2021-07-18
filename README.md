# mattro-rs

`mattro` is a **proc_macro** attribute parser for Rust.

## Usage
Add this to your Cargo.toml:
```
[dependencies]
mattro = "0.1.1"
```

You can parse:
- `Attribute` using `MacroAttribute::new(attribute)`
- `AttributeArgs` using `MacroAttribute::from_attribute_args(path, args, style)`

## Example
Parsing `AttributeArgs`:

`main.rs`
```rust
#[my_attribute(text="some text", number=120, array=1,2,3)]
fn main() {}
```

`lib.rs`
```rust
use mattro::MacroAttribute;
use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn my_attribute(attribute: TokenStream, item: TokenStream) -> TokenStream {
    let tokens = attribute.clone();
    let attribute_args: syn::AttributeArgs = syn::parse_macro_input!(tokens);

    // Creates a `MacroAttribute` using the `AttributeArgs`.
    let attr = MacroAttribute::from_attribute_args(
        // Path of the attribute
        "my_attribute",

        // The `AttributeArgs`
        attribute_args,

        // The attribute style `inner` or `outer`
        syn::AttrStyle::Outer
    );

    // Prints all the `MetaItem`s
    for meta_item in &attr {
        println!("{:#?}", meta_item);
    }

    // Returns the decorated item
    item
}
```

This prints out:
```scala
NameValue(
    NameValue {
        name: "text",
        value: Literal(
            Str(
                LitStr {
                    token: "some text",
                },
            ),
        ),
    },
)
NameValue(
    NameValue {
        name: "number",
        value: Literal(
            Int(
                LitInt {
                    token: 120,
                },
            ),
        ),
    },
)
NameValue(
    NameValue {
        name: "array",
        value: Array(
            [
                Int(
                    LitInt {
                        token: 1,
                    },
                ),
                Int(
                    LitInt {
                        token: 2,
                    },
                ),
                Int(
                    LitInt {
                        token: 3,
                    },
                ),
            ],
        ),
    },
)
```

### You could convert the attribute into a `name-value` pairs

```rust
// Converts the attribute into a `name-value` pairs
let name_values_attributes = attr.into_name_values().unwrap();

// Iterate over the `name-value` pairs
for (name, value) in &name_values_attributes {
    println!("{} => {}", name, value);
}
```

This prints out:
```js
text    => "some text"
number  => 120
array   => [1, 2, 3]
```