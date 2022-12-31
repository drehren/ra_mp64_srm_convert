# RetroArch N64 Save Converter

A simple converter for Retroarch's Mupen64Plus core save files; from eep, mpk, sra or fla to srm (or viceversa).

When a SRM file is split into its contents, only those with data will be exported.

## Usage TL;DR

### Group by filename (default)
```sh
$ ra_mp64_srm_convert A.srm B.mpk B.eep C.fla C.srm D.srm D.fla F.mpk1 F.mpk3
```

Output:
- A.eep, A.sra or A.fla, and/or A.mpk1 A.mpk2 A.mpk3 A.mpk4 (all new)
- B.srm (new)
- C.srm (updated)
- D.eep (updated) D.sra D.fla D.mpk1 D.mpk2 D.mpk3 D.mpk4 (all new)

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
$ ra_mp64_srm_convert -s B.mpk B.eep C.fla F.mpk3 C.srm
```

Output:
- B.eep C.sra C.fla B.mpk C.mpk2 F.mpk3 C.mpk4

## Usage

Given a set of input files, the program will:

1. Group them based on the file name
2. If no ```-c``` or ```-s``` flags: Convert them based on the first file of the group:
  - If the file is a SRM save file, then its contained saves will be dumped
    - New files will be created; existing files will overwritten only --overwrite was present
  - If the file is not an RA Mupen64Plus save, then a new RA Mupen64Plus save will be created
    - The contents will be from this first file and all others in the group

### Automatic mode:

Without arguments, the program will group all files based on their names (same names), and then
proceed to create SRM or split based on the type of the first file of the group.

For example
```sh
$ ra_mp64_srm_convert A.srm B.mpk B.eep C.fla C.srm D.srm D.fla F.mpk1 F.mpk3
```
will output the following (in the directory of the the first file for each group):
* A.srm -> A.eep, A.mpk1, A.mpk2, A.mpk3, A.mpk4, A.sra, A.fla (provided that there is actual data in each of the saves)
* B.mpk, B.eep -> B.srm (new srm file)
* C.fla, C.srm -> C.srm (srm updated with C.fla)
* D.srm, D.fla -> D.fla (fla updated from D.srm)
* F.mpk1 F.mpk2 -> F.srm

An *.mpk file will always be assigned to the first player controller pack.

### Forced mode

If one of the arguments ```-c``` or ```-s``` is passed, then the program will force a Creation or a 
Split, respectively. It will consider all files, irrespective of their names as inputs or outputs 
depending in their save type.

#### Create

Use ```-c``` or ```--create-srm```

For example
```sh
$ ra_mp64_srm_convert -c A.srm B.mpk B.eep C.fla C.srm D.srm D.fla F.mpk1 F.mpk3
```
will create an SRM at D.srm using B.eep, D.fla, F.mpk1, F.mpk3


#### Split

Use ```-s``` or ```--split-srm```

For example
```sh
$ ra_mp64_srm_convert -s A.srm B.mpk B.eep C.fla C.srm D.srm D.fla F.mpk1 F.mpk3
```
will split D.srm into B.eep, D.fla, F.mpk1, F.mpk3

## File save type detection:

1. If the file exists, then its size has to be one of the possible save sizes.
   - Mempack saves will be read and its internal checksums checked. If fail, the program will
     assume that the file is a SRAM save.
   - If the flag ```--merge-mempacks``` was set, and the checksum fails, then the program will 
     assume that the file is a SRM save.
2. If the file does not exist, then the program will check its extension.

### Type extension size table:

| Save type | Extension(s)          | Size(s)        |
|-----------|-----------------------|----------------|
| EEPROM    | *.eep                 | 512   B, 2 KiB |
| SRAM      | *.sra                 |  32 KiB        |
| FlashRAM  | *.fla                 | 128 KiB        |
| Mempack   | *.mpk, *.mpk(1,2,3,4) |  32 KiB        |

[Size source](http://micro-64.com/database/gamesave.shtml)

## Usage

To use it, simply drag and drop the file(s) into the program and it'll try to know what to do.

~~~~~~~
A simple converter for Retroarch's Mupen64Plus core save files

Usage: ra_mp64_srm_convert.exe [OPTIONS] <FILE>...

Arguments:
  <FILE>...  The input file(s)

Options:
  -v, --verbosity <VERBOSITY>    Sets the output verbosity [default: normal] [possible values: quiet, normal, debug]
  -c, --create-srm               Forces the creation of a SRM file from all the given files
  -s, --split-srm                Forces the split of an existing SRM to all the given files
      --overwrite                If set, the program will overwrite any existing files
      --change-endianness        Is set, any FlashRAM or SRAM data will swap its endianness
      --merge-mempacks           If set, the 4 memory pack files will be merged into one
      --output-dir <OUTPUT_DIR>  Sets the output directory for the created file (or files)
  -h, --help                     Print help information
  -V, --version                  Print version information
~~~~~~~

## Building

Requirements:
* rust >= 1.66

Simply use ```cargo``` to build:

```sh
$ cargo build --release
```

## License

MIT
