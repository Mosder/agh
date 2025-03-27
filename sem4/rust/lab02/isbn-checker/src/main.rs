use std::io;

fn main() {
    let mut user_input = String::new();
    println!("Podaj ISBN do sprawdzenia:");
    let _ = io::stdin().read_line(&mut user_input);
    println!("Poprawiny ISBN?: {}", check_isbn10(user_input));
}

fn check_isbn10(potential_isbn: String) -> bool {
    let char_array: Vec<char> = potential_isbn.trim().replace("-", "").chars().collect();
    if char_array.len() != 10 {return false};
    let digits_char_range = '0'..='9';
    let mut int_array: [u32; 10] = [0; 10];
    for i in 0..9 {
        if digits_char_range.contains(&char_array[i]) {
            int_array[i] = char_array[i].to_digit(10).unwrap();
        }
        else {return false};
    }
    if char_array[9] == 'X' {int_array[9] = 10}
    else if digits_char_range.contains(&char_array[9]) {int_array[9] = char_array[9].to_digit(10).unwrap()}
    else {return false};
    let mut weight = 10;
    let mut sum = 0;
    for int in int_array {
        sum += weight * int;
        weight -= 1;
    }
    return sum % 11 == 0;
}
