use msgpack_schema::*;
use msgpack_value::{msgpack, Bin, Ext, Value};
use proptest::prelude::*;

mod value {
    use super::*;

    pub fn serialize<S: Serialize>(x: &S) -> Value {
        let buf = crate::serialize(x).unwrap();
        crate::deserialize(&buf).unwrap()
    }

    pub fn deserialize<D: Deserialize>(value: Value) -> Result<D, DeserializeError> {
        let buf = crate::serialize(value).unwrap();
        crate::deserialize::<D>(&buf)
    }
}

#[test]
fn failing() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
}

#[test]
fn serialize_struct_tag() {
    #[derive(Serialize)]
    struct Human {
        #[tag = 0]
        age: u32,
        #[tag = 2]
        name: String,
    }

    let val = Human {
        age: 42,
        name: "John".into(),
    };
    assert_eq!(
        value::serialize(&val),
        Value::Map(vec![
            (Value::Int(0.into()), Value::Int(42.into())),
            (Value::Int(2.into()), Value::Str("John".to_owned().into()))
        ])
    );
}

#[test]
fn deserialize_struct_tag() {
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Human {
        #[tag = 0]
        age: u32,
        #[tag = 2]
        name: String,
    }

    let val = Value::Map(vec![
        (Value::Int(0.into()), Value::Int(42.into())),
        (Value::Int(2.into()), Value::Str("John".to_owned().into())),
    ]);

    assert_eq!(
        Human {
            age: 42,
            name: "John".into(),
        },
        value::deserialize(val).unwrap()
    );
}

#[test]
fn struct_tag_roundtrip() {
    #[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
    struct Human {
        #[tag = 0]
        age: u32,
        #[tag = 2]
        name: String,
    }

    let val = Human {
        age: 42,
        name: "John".into(),
    };

    assert_eq!(val, value::deserialize(value::serialize(&val)).unwrap());

    let val = Value::Map(vec![
        (Value::Int(0.into()), Value::Int(42.into())),
        (Value::Int(2.into()), Value::Str("John".to_owned().into())),
    ]);

    assert_eq!(
        val,
        value::serialize(&value::deserialize::<Human>(val.clone()).unwrap())
    );
}

#[test]
fn error_duplicate_tags() {
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Human {
        #[tag = 0]
        age: u32,
        #[tag = 2]
        name: String,
    }

    let val = msgpack!({
        0: 42,
        0: 43,
        2: "John",
    });

    assert_eq!(
        value::deserialize::<Human>(val).unwrap(),
        Human {
            age: 43,
            name: "John".to_owned()
        }
    );

    let val = msgpack!({
        0: true,
        0: 42,
        2: "John",
    });

    assert_eq!(
        value::deserialize::<Human>(val).unwrap(),
        Human {
            age: 42,
            name: "John".to_owned()
        }
    );

    let val = msgpack!({
        0: 42,
        0: true,
        2: "John",
    });

    assert!(matches!(
        value::deserialize::<Human>(val).unwrap_err(),
        DeserializeError::Validation(_)
    ));
}

#[test]
fn serialize_struct_optional() {
    #[derive(Serialize)]
    struct Human {
        #[tag = 0]
        age: u32,
        #[tag = 2]
        #[optional]
        name: Option<String>,
    }

    let val = Human {
        age: 42,
        name: Some("John".into()),
    };
    assert_eq!(
        value::serialize(&val),
        Value::Map(vec![
            (Value::Int(0.into()), Value::Int(42.into())),
            (Value::Int(2.into()), Value::Str("John".to_owned().into()))
        ])
    );

    let val = Human {
        age: 42,
        name: None,
    };
    assert_eq!(
        value::serialize(&val),
        Value::Map(vec![(Value::Int(0.into()), Value::Int(42.into())),])
    );
}

#[test]
fn deserialize_struct_optional() {
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Human {
        #[tag = 0]
        age: u32,
        #[tag = 2]
        #[optional]
        name: Option<String>,
    }

    let val = Value::Map(vec![
        (Value::Int(0.into()), Value::Int(42.into())),
        (Value::Int(2.into()), Value::Str("John".to_owned().into())),
    ]);
    assert_eq!(
        Human {
            age: 42,
            name: Some("John".into()),
        },
        value::deserialize(val).unwrap()
    );

    let val = Value::Map(vec![(Value::Int(0.into()), Value::Int(42.into()))]);
    assert_eq!(
        Human {
            age: 42,
            name: None,
        },
        value::deserialize(val).unwrap()
    );
}

#[test]
fn serialize_unit_variants() {
    #[derive(Serialize)]
    enum Animal {
        #[tag = 2]
        Dog,
    }

    let val = Animal::Dog;
    assert_eq!(value::serialize(&val), Value::Int(2.into()));
}

#[test]
fn deserialize_unit_variants() {
    #[derive(Deserialize, Debug, PartialEq, Eq)]
    enum Animal {
        #[tag = 2]
        Dog,
    }

    let val = Value::Int(2.into());
    assert_eq!(Animal::Dog, value::deserialize(val).unwrap());
}

#[test]
fn serialize_newtype_struct() {
    #[derive(Serialize)]
    struct S(u32);

    let val = S(42);
    assert_eq!(value::serialize(&val), Value::Int(42.into()));
}

#[test]
fn deserialize_newtype_struct() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct S(u32);

    let val = Value::Int(42.into());
    assert_eq!(S(42), value::deserialize(val).unwrap());
}

#[test]
fn serialize_empty_tuple_variants() {
    #[derive(Serialize)]
    enum Animal {
        #[tag = 2]
        Dog(),
    }

    let val = Animal::Dog();
    assert_eq!(value::serialize(&val), Value::Int(2.into()));
}

#[test]
fn deserialize_empty_tuple_variants() {
    #[derive(Deserialize, Debug, PartialEq, Eq)]
    enum Animal {
        #[tag = 2]
        Dog(),
    }

    let val = Value::Int(2.into());
    assert_eq!(Animal::Dog(), value::deserialize(val).unwrap());
}

#[test]
fn serialize_tuple_variants() {
    #[derive(Serialize, Debug, PartialEq, Eq)]
    enum Animal {
        #[tag = 1]
        Cat(String),
        #[tag = 2]
        Dog(u32),
    }

    assert_eq!(
        value::serialize(&Animal::Cat("hello".to_owned())),
        Value::Array(vec![1.into(), "hello".to_owned().into()])
    );

    assert_eq!(
        value::serialize(&Animal::Dog(42u32)),
        Value::Array(vec![2.into(), 42u32.into()])
    );
}

#[test]
fn deserialize_tuple_variants() {
    #[derive(Deserialize, Debug, PartialEq, Eq)]
    enum Animal {
        #[tag = 1]
        Cat(String),
        #[tag = 2]
        Dog(u32),
        #[tag = 3]
        Bird,
    }

    let val = Value::Int(3.into());
    assert_eq!(Animal::Bird, value::deserialize(val).unwrap());

    let val = Value::Int(1.into());
    assert!(value::deserialize::<Animal>(val).is_err());

    let val = Value::Int(10.into());
    assert!(value::deserialize::<Animal>(val).is_err());

    let val = Value::Array(vec![1.into(), 42u32.to_owned().into()]);
    assert!(value::deserialize::<Animal>(val).is_err());

    let val = Value::Array(vec![1.into(), "hello".to_owned().into()]);
    assert_eq!(
        Animal::Cat("hello".to_owned()),
        value::deserialize(val).unwrap()
    );
}

#[test]
fn serialize_untagged_enum() {
    #[derive(Serialize, Debug, PartialEq, Eq)]
    #[untagged]
    enum Animal {
        Cat(String),
        Dog(u32),
    }

    let val = Value::Int(3.into());
    assert_eq!(value::serialize(&Animal::Dog(3)), val);

    let val = Value::Str("hello".to_owned().into());
    assert_eq!(value::serialize(&Animal::Cat("hello".to_owned())), val);
}

#[test]
fn deserialize_untagged_enum() {
    #[derive(Deserialize, Debug, PartialEq, Eq)]
    #[untagged]
    enum Animal {
        Cat(String),
        Dog(u32),
    }

    let val = Value::Int(3.into());
    assert_eq!(Animal::Dog(3), value::deserialize(val).unwrap());

    let val = Value::Str("hello".to_owned().into());
    assert_eq!(
        Animal::Cat("hello".to_owned()),
        value::deserialize::<Animal>(val).unwrap()
    );

    let val = Value::Int((-10).into());
    assert!(value::deserialize::<Animal>(val).is_err());
}

#[test]
fn serialize_untagged_struct() {
    #[derive(Serialize, Debug, PartialEq, Eq)]
    #[untagged]
    struct Human {
        name: String,
        age: u32,
    }

    let val = Human {
        name: "John".to_string(),
        age: 42,
    };
    assert_eq!(value::serialize(&val), msgpack!(["John", 42]));
}

#[test]
fn deserialize_untagged_struct() {
    #[derive(Deserialize, Debug, PartialEq, Eq)]
    #[untagged]
    struct Human {
        name: String,
        age: u32,
    }

    let val = msgpack!(["John", 42]);
    assert_eq!(
        Human {
            name: "John".to_string(),
            age: 42,
        },
        value::deserialize(val).unwrap()
    );

    let val = msgpack!(["John", 42, nil]);
    assert!(value::deserialize::<Human>(val).is_err());
}

fn arb_value() -> impl Strategy<Value = Value> {
    let leaf = prop_oneof![
        Just(Value::Nil),
        any::<bool>().prop_map(|v| v.into()),
        any::<u64>().prop_map(|v| v.into()),
        any::<i64>().prop_map(|v| v.into()),
        any::<f32>().prop_map(|v| v.into()),
        any::<f64>().prop_map(|v| v.into()),
        ".*".prop_map(|v| v.into()),
        ".*".prop_map(|v| Bin(v.into_bytes()).into()),
        any::<i8>().prop_flat_map(|tag| ".*".prop_map(move |v| Value::Ext(Ext {
            r#type: tag,
            data: v.into_bytes()
        }))),
    ];
    leaf.prop_recursive(
        4,   // 4 levels deep
        128, // Shoot for maximum size of 128 nodes
        5,   // We put up to 5 items per collection
        |inner| {
            prop_oneof![
                // Take the inner strategy and make the two recursive cases.
                prop::collection::vec(inner.clone(), 0..=4).prop_map(|v| v.into()),
                prop::collection::vec((inner.clone(), inner), 0..=4).prop_map(|v| v.into()),
            ]
        },
    )
}

proptest! {
    #[test]
    fn roundtrip_binary(v in arb_value()) {
        let buf = msgpack_schema::serialize(&v).unwrap();
        assert_eq!(v, msgpack_schema::deserialize(buf.as_slice()).unwrap());
    }
}

#[test]
fn serialize_struct_tag_schema() {
    #[derive(Serialize)]
    struct Human {
        #[schema(0)]
        age: u32,
        #[schema(2)]
        name: String,
    }

    let val = Human {
        age: 42,
        name: "John".into(),
    };
    assert_eq!(
        value::serialize(&val),
        Value::Map(vec![
            (Value::Int(0.into()), Value::Int(42.into())),
            (Value::Int(2.into()), Value::Str("John".to_owned().into()))
        ])
    );
}

#[test]
fn serialize_struct_flatten() {
    #[derive(Serialize)]
    struct S1 {
        #[tag = 1]
        x: u32,
    }

    #[derive(Serialize)]
    struct S2 {
        #[tag = 2]
        x: u32,
        #[flatten]
        s1: S1,
    }

    let val = S2 {
        x: 42,
        s1: S1 { x: 43 },
    };
    assert_eq!(
        value::serialize(&val),
        Value::Map(vec![
            (Value::Int(2.into()), Value::Int(42.into())),
            (Value::Int(1.into()), Value::Int(43.into()))
        ])
    );
}

#[test]
fn deserialize_struct_flatten() {
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct S1 {
        #[tag = 1]
        x: u32,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct S2 {
        #[tag = 2]
        x: u32,
        #[flatten]
        s1: S1,
    }

    let val = S2 {
        x: 42,
        s1: S1 { x: 43 },
    };
    assert_eq!(
        val,
        value::deserialize(Value::Map(vec![
            (Value::Int(2.into()), Value::Int(42.into())),
            (Value::Int(1.into()), Value::Int(43.into()))
        ]))
        .unwrap()
    );
}

#[test]
fn serialize_deserialize_empty() {
    #[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
    struct Empty {}

    let empty = Empty {};

    assert_eq!(value::serialize(&empty), msgpack!({}));

    assert_eq!(empty, value::deserialize(msgpack!({})).unwrap());
}

#[test]
fn serialize_tuple_struct() {
    #[derive(Serialize, Debug, PartialEq, Eq)]
    struct S(u32, String);

    let s = S(42, "hello".to_owned());
    assert_eq!(value::serialize(&s), msgpack!([42, "hello"]));
}

#[test]
fn deserialize_tuple_struct() {
    #[derive(Deserialize, Debug, PartialEq, Eq)]
    struct S(u32, String);

    let s = S(42, "hello".to_owned());
    assert_eq!(s, value::deserialize(msgpack!([42, "hello"])).unwrap());
}

#[test]
fn deserialize_tuple_struct_wrong_length() {
    #[allow(dead_code)]
    #[derive(Deserialize, Debug)]
    struct S(u32, bool);

    let v = msgpack!([42]);

    assert!(matches!(
        value::deserialize::<S>(v).unwrap_err(),
        msgpack_schema::DeserializeError::Validation(_)
    ));
}

#[test]
fn regression_test_variable_capture() {
    #[derive(Deserialize, Serialize, Debug, PartialEq, Eq)]
    struct S {
        #[tag = 0]
        value: i32,
        #[tag = 1]
        serializer: i32,
        #[tag = 2]
        deserializer: i32,
        #[tag = 3]
        tag: i32,
        #[tag = 4]
        max_len: i32,
        #[tag = 5]
        len: i32,
        #[tag = 6]
        e: i32,
        #[tag = 7]
        count: i32,
        #[tag = 8]
        v: i32,
        #[tag = 9]
        ident: i32,
        #[tag = 10]
        r#type: i32,
    }
    let v = msgpack!({
        0:123,
        1:456,
        2:789,
        3:123,
        4:456,
        5:789,
        6:123,
        7:456,
        8:789,
        9:123,
        10:456,
    });
    assert!(matches!(
        value::deserialize::<S>(v).unwrap(),
        S {
            value: 123,
            serializer: 456,
            deserializer: 789,
            tag: 123,
            max_len: 456,
            len: 789,
            e: 123,
            count: 456,
            v: 789,
            ident: 123,
            r#type: 456,
        }
    ));
}

#[test]
fn serialize_with_skipped_fields() {
    #[derive(Serialize, Debug, PartialEq, Eq)]
    struct Human {
        #[tag = 0]
        age: u32,
        #[skip]
        name: String,
    }

    let val = Human {
        age: 42,
        name: "John".to_string(),
    };

    assert_eq!(
        value::serialize(&val),
        Value::Map(vec![(Value::Int(0.into()), Value::Int(42.into())),])
    );
}

#[allow(unused_variables)]
#[test]
fn deserialize_with_skipped_fields() {
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Human {
        #[tag = 0]
        age: u32,
        #[skip]
        name: String,
    }

    let val = Value::Map(vec![
        (Value::Int(0.into()), Value::Int(42.into())),
        (Value::Int(2.into()), Value::Str("John".to_owned().into())),
    ]);

    assert_eq!(
        Human {
            age: 42,
            name: "".into(),
        },
        value::deserialize(val).unwrap()
    );
}

#[test]
fn deserialize_with_default_fields() {
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Human {
        #[tag = 0]
        age: u32,
        #[tag = 1]
        #[default = "abc"]
        name: String,
        #[tag = 2]
        #[default = "default_true"]
        passed: bool,
    }

    fn default_true() -> bool {
        true
    }
    fn abc() -> String {
        "abc".to_string()
    }

    let val = Value::Map(vec![(Value::Int(0.into()), Value::Int(42.into()))]);

    assert_eq!(
        Human {
            age: 42,
            name: "abc".into(),
            passed: true,
        },
        value::deserialize(val).unwrap()
    );
}

#[test]
fn serialize_enum_with_named_fields() {
    #[derive(Serialize, Debug, PartialEq, Eq)]
    enum Data {
        #[tag = 0]
        UserInfo {
            #[tag = 0]
            age: u32,
            #[tag = 1]
            name: String,
        },
    }

    let val = Data::UserInfo {
        age: 42,
        name: "John".to_string(),
    };

    assert_eq!(
        value::serialize(&val),
        Value::Array(vec![
            Value::Int(0.into()),
            Value::Map(vec![
                (Value::Int(0.into()), Value::Int(42.into())),
                (Value::Int(1.into()), Value::Str("John".to_owned().into()))
            ])
        ])
    );
}

#[test]
fn deserialize_enum_with_named_fields() {
    #[derive(Deserialize, Debug, PartialEq, Eq)]
    enum Data {
        #[tag = 0]
        UserInfo {
            #[tag = 0]
            age: u32,
            #[tag = 1]
            name: String,
        },
    }

    let val = Value::Array(vec![
        Value::Int(0.into()),
        Value::Map(vec![
            (Value::Int(0.into()), Value::Int(42.into())),
            (Value::Int(1.into()), Value::Str("John".to_owned().into())),
        ]),
    ]);

    assert_eq!(
        Data::UserInfo {
            age: 42,
            name: "John".to_string(),
        },
        value::deserialize(val).unwrap()
    );
}
