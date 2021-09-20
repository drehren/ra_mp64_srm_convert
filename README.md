RetroArch N64 Save Converter
============================

A "converter" of RA Mupen64+ n64 save files: from eep, mpk, sra or fla to srm (or viceversa).

You can pass multiple files and it'll separate per file name, and then from their extensions the program will figure out what to do.

For example if you do:
```sh
$ ra_mp64_srm_convert A.srm B.mpk B.eep C.fla C.srm D.srm D.fla
```
The program will output the following in the directory where the first of each file was:
* A.srm -> A.eep, A1.mpk, A2.mpk, A3.mpk, A4.mpk, A.sra, A.fla (provided that there is actual data in each of the saves)
* B.mpk, B.eep -> B.srm
* C.fla, C.srm -> C.srm (updated with C.fla)
* D.srm, D.fla -> D.fla (updated from D.srm)

## Usage

To use it, simply drag and drop the file(s) into the program and it'll try to know what to do.

    USAGE:
        ra_mp64_srm_convert.exe [FLAGS] [OPTIONS] <files>...

    FLAGS:
        -h, --help         Prints help information
            --overwrite    If set, the program can overwrite an existing filesystem files
        -V, --version      Prints version information

    OPTIONS:
            --output-dir <output-dir>    Specify the output directory for the created file (or files)

    ARGS:
        <files>...    The input file(s). It can be *.srm (to extract), or *.sra, *.fla, *.eep or *.mpk (to create, based on file name)


## Building

Requirements:
* rust >= 1.55 (tested, nothing fancy used so I think any 2018 edition should work)

Simply use ```cargo``` to build:

```sh
$ cargo build --release
```


## License

MIT
