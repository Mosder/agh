use std::f64::consts::PI;

mod vec2d;

fn main() {
    let v1 = vec2d::Vec2D::unit_vector(PI/4.0);
    let v2 = vec2d::Vec2D::unit_vector(PI);
    println!("v1:");
    v1.print();
    println!("v2:");
    v2.print();
    println!("v1 == v2:");
    println!("{}", v1.equals(&v2));
    println!("v1 + v2:");
    (v1.clone() + v2.clone()).print();
    println!("v1 - v2:");
    (v1.clone() - v2.clone()).print();
    println!("3*v1:");
    v1.scalar_mult(3.0).print();
    println!("v1 dot v2:");
    println!("{}", v1.dot_product(&v2));
}
