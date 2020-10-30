fn add(a:i32, b:i32) -> i32 {
    a+b
}

fn sub(a:i32, b:i32) -> i32 {
    a-b
}

fn main() {
    let mut a = 1;
    let b = 2;
    a = sub(a, b);
    a = add(a, -1);
    a + b
}