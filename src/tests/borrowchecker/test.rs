fn main() {
    let mut a = 3;
    let b = &a;
    a = 3;

    //println!("{:?}", a);
    println!("{:?}", b);
}