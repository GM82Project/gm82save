use time::OffsetDateTime;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let date = OffsetDateTime::now_utc();
    println!("cargo:rustc-env=BUILD_DATE={} {}, {}", date.month(), date.day(), date.year());
    Ok(())
}
