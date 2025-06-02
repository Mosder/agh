use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::Path;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use anyhow::Result;
 
const DATA_FILE : &str = "data.json";
 
const API_QUERY : &str = "https://api.openf1.org/v1/car_data?driver_number=55&session_key=9159";

const SAMPLE_LENGTH: f32 = 1.0/3.7;
 
#[derive(Serialize, Deserialize, Debug)]
struct CarData {
    brake : u32,
    date: DateTime<Utc>,
    driver_number: u32,
    drs: u8,
    meeting_key: u32,
    n_gear: u8,
    rpm: u32,
    session_key: u32,
    speed: u32,
    throttle: u8
}
 
fn fetch_car_data(url: &str, data_file : &str) -> Result<()> {
    let data = reqwest::blocking::get(url)?.text()?;
    let car_data : Vec<CarData> = serde_json::from_str(&data)?;
    let writer = BufWriter::new(File::create(data_file)?);
    serde_json::to_writer(writer, &car_data)?;
    Ok(())
}
 
fn load_car_data_from_file(data_file : &str) -> Result<Vec<CarData>> {
    let car_data : Vec<CarData> = serde_json::from_reader(BufReader::new(File::open(data_file)?))?;
    Ok(car_data)
}
 
fn check_if_data_exists(data_file : &str) -> bool {
    Path::new(data_file).exists()
}

fn average(car_data: &Vec<CarData>, session_key: u32, driver_number: u32) -> f32 {
    let speed_values: Vec<u32> = car_data.iter()
        .filter(|&data| data.session_key == session_key && data.driver_number == driver_number)
        .map(|data| data.speed)
        .collect::<Vec<u32>>();
    return speed_values.iter().sum::<u32>() as f32 / speed_values.iter().count() as f32;
}

fn max_rpm(car_data: &Vec<CarData>) -> (u32, u8) {
    let max_rpm_moment: &CarData = car_data.iter()
        .max_by_key(|data| data.rpm)
        .unwrap();
    return (max_rpm_moment.rpm, max_rpm_moment.n_gear);
}

fn high_speed(car_data: &Vec<CarData>, speed: u32) -> f32 {
    return SAMPLE_LENGTH * car_data.iter()
        .filter(|&data| data.speed > speed)
        .count() as f32;
}

fn main() -> Result<()> {
 
    if !check_if_data_exists(DATA_FILE) {
        fetch_car_data(API_QUERY, DATA_FILE)?;
    }
 
    let car_data = load_car_data_from_file(DATA_FILE).unwrap();
    
    println!("Average speed (km/h): {}", average(&car_data, 9159, 55));
    println!("Time (in s) when speed > 300: {}", high_speed(&car_data, 300));
    println!("Maximum rpm (rpm, gear): {:?}", max_rpm(&car_data));
 
    Ok(())
} 
