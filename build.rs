use time::OffsetDateTime;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let date = OffsetDateTime::now_utc();
    println!("cargo:rustc-env=ABOUT_BUILD_DATE={} {}, {}", date.month(), date.day(), date.year());
    println!(
        "cargo:rustc-env=ERROR_BUILD_DATE={}{:02}{:02}T{:02}{:02}",
        date.year(),
        u8::from(date.month()),
        date.day(),
        date.hour(),
        date.minute()
    );
    Ok(())
}
