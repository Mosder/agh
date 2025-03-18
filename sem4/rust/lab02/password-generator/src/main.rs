use rand::Rng;
use std::collections::HashMap;

fn main() {
    let set1 = "lowercase";
    let set2 = "uppercase";
    let set3 = "digits";
    let set4 = "special";
    generate_password(6, &[set1, set3]);
}

fn generate_password(length: usize, charsets: &[&str]) -> String {
    let mut chars_map: HashMap<&str, &str> = HashMap::new();
    chars_map.insert("lowercase", "abcdefghijklmnopqrstuvwxyz");
    chars_map.insert("uppercase", "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    chars_map.insert("digits", "0123456789");
    chars_map.insert("special", "!@#$%^&*()-_=+\\|[]{};:/?,<.>");

    let mut valid_chars = String::new();
    for charset in charsets {
        if chars_map.contains_key(charset) {
            valid_chars.push_str(chars_map.get(charset).unwrap());
        }
    }
    println!("{}", valid_chars);


    let mut rng = rand::rng();
    let mut password = String::new();

    return password;
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn test_password_length() {
        let length: usize = 12;
        assert_eq!(generate_password(length).len(), length);
    }
}