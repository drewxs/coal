use std::{
    fs::File,
    io::{BufReader, BufWriter, Read, Write},
    process,
};

pub fn read_file_to_str(path: &str) -> String {
    let file = File::open(path).unwrap_or_else(|_| {
        eprintln!("file not found: {path}");
        process::exit(1);
    });

    let mut input = String::new();
    BufReader::new(file)
        .read_to_string(&mut input)
        .unwrap_or_else(|_| {
            eprintln!("failed to read file: {path}");
            process::exit(1);
        });

    input
}

pub fn read_file_to_bytes(path: &str) -> Vec<u8> {
    let file = File::open(path).unwrap_or_else(|_| {
        eprintln!("file not found: {path}");
        process::exit(1);
    });

    let mut input = Vec::new();
    BufReader::new(file)
        .read_to_end(&mut input)
        .unwrap_or_else(|_| {
            eprintln!("failed to read file: {path}");
            process::exit(1);
        });

    input
}

pub fn write_str_to_file(path: &str, s: &str) {
    let file = File::create(path).unwrap_or_else(|_| {
        eprintln!("failed to create file: {path}");
        process::exit(1);
    });

    let mut writer = BufWriter::new(file);

    writer.write_all(s.as_bytes()).unwrap_or_else(|_| {
        eprintln!("failed to write to file: {path}");
        process::exit(1);
    });

    writer.flush().unwrap();
}

pub fn write_bytes_to_file(path: &str, bytes: &[u8]) {
    let file = File::create(path).unwrap_or_else(|_| {
        eprintln!("failed to create file: {path}");
        process::exit(1);
    });

    let mut writer = BufWriter::new(file);

    writer.write_all(bytes).unwrap_or_else(|_| {
        eprintln!("failed to write to file: {path}");
        process::exit(1);
    });

    writer.flush().unwrap();
}
