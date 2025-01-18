use std::{fs, io};

const HELLO_TEMPLATE: &str = include_str!("../../templates/hello.coal");

pub fn new(name: &str) -> io::Result<String> {
    fs::create_dir(name)?;
    fs::write(format!("{name}/main.coal"), HELLO_TEMPLATE)?;

    Ok(format!("Project created `{name}`"))
}
