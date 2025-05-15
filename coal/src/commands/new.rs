use std::{fs, io};

const HELLO_TEMPLATE: &str = include_str!("../../templates/hello.coal");

pub fn new(name: &str) -> io::Result<String> {
    fs::create_dir_all(format!("{name}/src"))?;
    fs::write(format!("{name}/src/main.coal"), HELLO_TEMPLATE)?;

    Ok(format!("Project created `{name}`"))
}
