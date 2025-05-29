# CNP-Validator-In-Assembly

## How it works: 
After calling it, the program launches up in the terminal, prompting you to pick between two options : 

    1. One cnp at a time keyboard-insert mode
    2. Multiple cnp's at once file-reading mode.
    
### In the keyboard-insert mode:

The user can enter as many CNP's as they want. Once they press the "Q" key, the program quits.

### In the File reading mode : 

The user must first enter their target file ( along with the extension).

The program checks if the file can be found, read and/or accessed. If so, it proceeds to checking each cnp in the given file.

After checking all cnp's, it concatenates the result next to each cnp, and, at the end of the file, several statistics such as "Male / Female" count or "Age group" count.

## how to run, in case you don't usually have a MS-DOS emulator running around in your computer:
1. Run the installer TASM installer.
2. Goto C:\Tasm 1.4\Tasm (ish, something like that)
3. Move files from "files" there, so that MSDOS can see those.
4. Open "Tasm 1.4 Windows 7 ..." shortcut from desktop (after instalation).
5. MSDOS boots up.
6. Write "main" in MSDOS terminal (-yeah, yeah I just left it like that.)
7. Use CNP.txt for file testing
8. Use dir/cd/ etc commands when in need to explore/fix.

I don't own the rights to said emulator. Only the provided assembly code belongs to me.
