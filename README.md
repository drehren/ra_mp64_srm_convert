RetroArch N64 Save Converter
============================

A "converter" of RA Mupen64+ n64 save files: from eep, mpk, sra or fla to srm (or viceversa).

You can pass multiple files and it'll separate per file name, and then from their extensions the program will figure out what to do.

For example if you do:
```sh
$ ra_mp64_srm_convert A.srm B.mpk B.eep C.fla C.srm D.srm D.fla
```
The program will output the following in the directory where the first of each file was:
* A.srm -> A.eep, A.1.mpk, A.2.mpk, A.3.mpk, A.4.mpk, A.sra, A.fla (provided that there is actual data in each of the saves)
* B.mpk, B.eep -> B.srm
* C.fla, C.srm -> C.srm (updated with C.fla)
* D.srm, D.fla -> D.fla (updated from D.srm)

## Usage

To use it, simply drag and drop the file(s) into the program and it'll try to know what to do.

    ra_mp64_srm_convert 0.10.0
    A simple converter for Retroarch's Mupen64 core save files.

    It (tries) to detect the save file and does one of the following:
    - .eep|.fla|*.mpk|.sav: group based on the file names and creates and .srm file.
    - .srm                : extract save data and creates .eep, .fla, .*.mpk, .sav file(s).

    USAGE:
        ra_mp64_srm_convert [OPTIONS] <FILES>...

    ARGS:
        <FILES>...
                The input file(s). It can be *.srm (to extract), or *.sra, *.fla, *.eep or *.mpk (to
                create, based on file name)

    OPTIONS:
        -h, --help
                Print help information

            --log-file <LOG_FILE>
                Logs to the specified file

            --output-dir <OUTPUT_DIR>
                Specify the output directory for the created file (or files)

            --merge-mempacks
                If set, the controller pack memory files will be merged into one

            --overwrite
                If set, the program can overwrite an existing filesystem files

        -q, --quiet
                Set this to suppress all output

        -v, --verbose
                Sets the verbose output (the more, the merrier!)

        -V, --version
                Print version information


## Building

Requirements:
* rust >= 1.55 (tested, nothing fancy used so I think any 2018 edition should work)

Simply use ```cargo``` to build:

```sh
$ cargo build --release
```


## License

MIT
