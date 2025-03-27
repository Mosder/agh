use rand::Rng;
use std::collections::{HashMap, HashSet};

fn main() {
    let length: usize = 12;
    let set1 = "lowercase";
    let set2 = "uppercase";
    let set3 = "digits";
    let set4 = "special";
    let password_sets: [&[&str]; 5] = [&[set1], &[set2], &[set3], &[set4], &[]];
    for set in password_sets {
        let password = generate_password(length, set);
        let valid_chars = get_valid_chars(set);
        let valid_chars_set: HashSet<char> = valid_chars.chars().collect();
        let password_chars_set: HashSet<char> = password.chars().collect();
        println!("password: {}\nallowed characters: {}", password, valid_chars);
        println!("password is a subset of allowed characters: {}\n", password_chars_set.is_subset(&valid_chars_set));
    }
}

fn get_valid_chars(charsets: &[&str]) -> String {
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
    if valid_chars.is_empty() {
        for (_, chars) in &chars_map {
            valid_chars.push_str(chars);
        }
    }
    return valid_chars;
}

fn generate_password(length: usize, charsets: &[&str]) -> String {
    let valid_chars = get_valid_chars(charsets);

    let mut rng = rand::rng();
    let mut password = String::new();
    for _ in 0..length {
        password.push(valid_chars.chars().nth(rng.random_range(0..valid_chars.len())).unwrap());
    }

    return password;
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn test_password_length() {
        let length: usize = 12;
        assert_eq!(generate_password(length, &[]).len(), length);
    }

    #[test]
    fn test_password_chars() {
        let length: usize = 12;
        let password_count = 100;
        let charsets = ["lowercase", "uppercase", "digits", "special"];
        let mut rng = rand::rng();
        for _ in 0..password_count {
            let mut used_charsets: Vec<&str> = Vec::new();
            for i in 0..charsets.len() {
                if rng.random_bool(0.5) {
                    used_charsets.push(charsets[i]);
                }
            }
            let password = generate_password(length, &used_charsets);
            let valid_chars_set: HashSet<char> = get_valid_chars(&used_charsets).chars().collect();
            let used_chars_set: HashSet<char> = password.chars().collect();
            assert_eq!(valid_chars_set.is_superset(&used_chars_set), true);
        }
    }
}