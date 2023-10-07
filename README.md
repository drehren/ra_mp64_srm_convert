# RetroArch N64 Save Converter

A simple converter for Retroarch's Mupen64Plus core save files; from eep, mpk*, sra or fla to srm (or viceversa).

When a SRM file is split into its contents, only those contents with data will be created.

This is a console application. For a graphical one see [ramp64-convert-gui](https://github.com/drehren/ramp64-convert-gui), for a web-based one see [ramp64-convert-web](https://github.com/drehren/ramp64-convert-web).

## Usage TL;DR

### Group by filename (default)
```sh
$ ra_mp64_srm_convert A.srm B.mpk B.eep C.fla C.srm D.srm D.fla F.mpk1 F.mpk3
```

Output:
- A.eep, A.sra or A.fla, and/or A.mpk1, A.mpk2, A.mpk3, A.mpk4 (all new)
- B.srm (new)
- C.srm (updated if exists and `--overwrite` is set)
- D.eep (overwritten if `--overwrite`), D.sra and/or D.fla; D.mpk1 D.mpk2 D.mpk3 D.mpk4 (all new)
- F.srm (new)

Note: This program will only overwrite existing files if `--overwrite` is given.

### Create a SRM

#### Example 1

```sh
$ ra_mp64_srm_convert -c B.mpk B.eep C.fla F.mpk3
```

Output:
- B.srm

#### Example 2

```sh
$ ra_mp64_srm_convert -c B.mpk B.eep C.fla F.mpk3 C.srm
```

Output:
- C.srm


### Split a SRM

#### Example 1

```sh
$ ra_mp64_srm_convert -s A.srm
```

Output:
- A.eep A.sra A.fla A.mpk1 A.mpk2 A.mpk3 A.mpk4

#### Example 2

```sh
$ ra_mp64_srm_convert -s B.mpk B.eep C.fla F.mpk3 A.srm
```

Output:
- A.eep A.sra A.fla A.mpk1 A.mpk2 A.mpk3 A.mpk4

## Usage

Given a set of input files, the program will:

1. Group them based on the file name
2. If no ```-c``` or ```-s``` flags: Convert them based on the first file of the group:
  - If the file is a SRM save file, then its contained saves will be dumped
    - New files will be created; existing files will overwritten only --overwrite was present
  - If the file is not an RA Mupen64Plus save, then a new RA Mupen64Plus save will be created
    - The contents will be from this first file and all others in the group
3. Create the new file(s).
  - If there are existing files, the group will fail at that point unless ```--overwrite``` is set

### Automatic mode:

Without arguments, the program will group all files based on their names (same names), and then
proceed to create SRM or split based on the type of the first file of the group.

For example
```sh
$ ra_mp64_srm_convert A.srm B.mpk B.eep C.fla C.srm D.srm D.sra F.mpk1 F.mpk3
```
will output the following:
* A.srm -> A.eep, A.mpk1, A.mpk2, A.mpk3, A.mpk4, A.sra, A.fla (provided that there is actual data in each of the saves)
* B.mpk, B.eep -> B.srm (srm created)
* C.fla, C.srm -> C.srm (srm created/updated with C.fla) (needs --overwrite to update C.srm)
* D.srm, D.sra -> D.sra (sra created/updated from D.srm) (needs --overwrite to update D.sra)
* F.mpk1 F.mpk2 -> F.srm (srm created)

An *.mpk file will always be assigned to the first player controller pack.

### Forced mode

If one of the arguments ```-c``` or ```-s``` is passed, then the program will force a Creation or a 
Split, respectively. It will consider all files, irrespective of their names as inputs or outputs 
depending in their save type.

This will only operate for a single SRM file.

#### Create

Use ```-c``` or ```--create-srm```

Inputs: *.eep, *.fla, *.sra and/or *.mpk(1-4)

Output: *.srm

For example
```sh
$ ra_mp64_srm_convert -c A.srm B.mpk B.eep C.fla C.srm D.srm D.fla F.mpk1 F.mpk3
```
will create a SRM at D.srm using B.eep, D.fla, F.mpk1, F.mpk3


#### Split

Use ```-s``` or ```--split-srm```

Input: *.srm

Outputs: *.eep, *.fla, *.sra and/or *.mpk(1-4)

For example
```sh
$ ra_mp64_srm_convert -s A.srm B.mpk B.eep C.fla C.srm D.srm D.fla F.mpk1 F.mpk3
```
will split D.srm into D.sra, B.eep, D.fla, F.mpk1, D.mpk2, F.mpk3 and/or D.mpk4

## File save type detection:

1. If the file exists, then its size has to be one of the possible save sizes.
   - Mempack saves will be read and its internal checksums checked. If fail, the program will
     assume that the file is a SRAM save.
   - If the flag ```--merge-mempacks``` was set, and the checksum fails, then the program will 
     assume that the file is a SRM save.
2. If the file does not exist, then the program will check its extension.

### Type extension size table:

| Save type       | Extension(s)          | Size(s)        |
|-----------------|-----------------------|----------------|
| N64 - EEPROM    | *.eep                 | 512   B, 2 KiB |
| N64 - SRAM      | *.sra                 |  32 KiB        |
| N64 - FlashRAM  | *.fla                 | 128 KiB        |
| N64 - Mempack   | *.mpk, *.mpk(1,2,3,4) |  32 KiB        |
| RA  - SRM       | *.srm                 | 290 KiB        |

[N64 Save types size source](http://micro-64.com/database/gamesave.shtml)

## Help

~~~~~~~
A simple converter for Retroarch's Mupen64Plus core save files

Usage: ra_mp64_srm_convert.exe [OPTIONS] [FILE]...

Arguments:
  [FILE]...  The input file(s)

Options:
  -v, --verbosity <VERBOSITY>    Sets the output verbosity [default: normal] [possible values: quiet, normal, debug]
  -c, --create-srm               Forces the creation of a SRM file from all the given files
  -s, --split-srm                Forces the split of an existing SRM to all the given files
  -m, --merge-mempacks           When splitting a SRM, merge all controller packs in one file
      --overwrite                If set, any existing file will be overwritten
  -o, --output-dir <OUTPUT_DIR>  Sets the output directory
      --change-endianness        If set, EEP and FlashRAM bytes will be swapped
  -i, --input-dir <INPUT_DIR>    Use this flag to convert files from directory
  -h, --help                     Print help
  -V, --version                  Print version
~~~~~~~

## Building

Requirements:
* rust >= 1.68

Simply use ```cargo``` to build:

```sh
$ cargo build --release
```

## License

MIT
