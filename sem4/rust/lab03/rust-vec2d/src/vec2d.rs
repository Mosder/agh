use std::ops::{Add, Sub};

#[derive(Clone)]
pub struct Vec2D {
    x: f64,
    y: f64
}

impl Vec2D {    
    pub fn new_vector(x: f64, y: f64) -> Vec2D {
        Vec2D{x, y}
    }

    pub fn unit_vector(angle: f64) -> Vec2D {
        Vec2D{x: 1.0 * f64::cos(angle), y: 1.0 * f64::sin(angle)}
    }

    pub fn print(&self) {
        println!("Vector 2D - x: {:.5}, y: {:.5}", self.x, self.y);
    }

    pub fn equals(&self, other: &Vec2D) -> bool {
        if f64::abs(self.x - other.x) < 1e-12 && f64::abs(self.y - other.y) < 1e-12 {true}
        else {false}
    }

    pub fn scalar_mult(&self, scalar: f64) -> Vec2D {
        Vec2D::new_vector(self.x * scalar, self.y * scalar)
    }

    pub fn dot_product(&self, other: &Vec2D) -> f64 {
        self.x * other.x + self.y * other.y
    }
}

impl Add for Vec2D {
    type Output = Self;
 
    fn add(self, other: Vec2D) -> Self {
        Vec2D::new_vector(self.x + other.x, self.y + other.y)
    }
}
 
impl Sub for Vec2D {
    type Output = Self;
 
    fn sub(self, other: Vec2D) -> Self {
        Vec2D::new_vector(self.x - other.x, self.y - other.y)
    }
}