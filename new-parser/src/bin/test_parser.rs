use {
    std::sync::Arc,
    new_parser::{Parser, Span, Spanned},
};

fn main() {
    let program = new_parser::program();
    let dir = {
        let mut dir = std::env::current_dir().unwrap();
        while dir.file_name().unwrap() != "sway" {
            dir.pop();
        }
        dir.push("test");
        dir.push("src");
        dir.push("e2e_vm_tests");
        dir.push("test_programs");
        dir
    };
    let mut good = 0;
    let mut bad = 0;
    for entry_res in walkdir::WalkDir::new(&dir).sort_by_file_name() {
        let entry = entry_res.unwrap();
        if !entry.file_type().is_file() {
            continue;
        }
        let path = entry.path();
        match path.extension() {
            Some(extension) if extension == "sw" => (),
            _ => continue,
        }
        if {
            path.to_str().unwrap().contains("parsing_error") ||
            path.to_str().unwrap().contains("top_level_vars") 
        } {
            continue;
        }

        let src = {
            let src = std::fs::read(path).unwrap();
            let src = String::from_utf8(src).unwrap();
            let len = src.len();
            let src = Arc::from(src);
            Span::new(src, 0, len)
        };
        
        println!("parsing: {}", path.display());
        let (_commited, res) = {
            program
            .clone()
            .parse(&src)
        };
        match res {
            Ok(_parsed) => {
                println!("    -> ok");
                good += 1;
            },
            Err(error) => {
                println!("{:?}", error);
                let span = error.span();
                println!("{:?}", span.as_str());
                println!("{:?}", &span.src()[span.start()..]);
                bad += 1;

                let report = error.report();
                report.print(new_parser::ReportingCache::new()).unwrap();
                break;
            },
        }
    }
    println!("good == {}", good);
    println!("bad == {}", bad);
}