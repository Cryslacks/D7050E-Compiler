fn a(x: bool, y: bool) -> bool {
    if x && y {
        let a: bool = true;
        y || a
    } else {
        x && false
    }
}
fn b(x: bool, y: bool) -> i32 {
    let a: bool = a(x, y || false);
    let mut b: i32 = 0;
    if a && y {
        let a: bool = true;
        if y || a {
            b = b + 1;
        };
    } else {
        if !(x && false) {
            b = b - 1;
        }
    };
    b + 3
}
fn c(x: bool, y: bool) -> i32 {
    let mut b: i32 = 0;
    let mut c: i32 = 1;
    while (b < 10) {
        c = c * 2;
    }
    c
}