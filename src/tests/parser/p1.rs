fn a() -> () {
    let _a: i32 = 5;
}
fn b(_x: i32, _y: i32) -> i32 {
    3 
}
fn c(x: i32, y: i32) -> i32 {
    let a: i32 = 5;
    let b: i32 = x + y;
    -a - (-b) * y
}