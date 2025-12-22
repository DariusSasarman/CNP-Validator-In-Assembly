# CNP-Validator-In-Assembly

## Here's a video of it in action:
[Screencast from 2025-12-22 21-17-17.webm](https://github.com/user-attachments/assets/10396236-a768-47a0-bf74-4ad990c53d06)

## Here's how a batch-processed file looks:

<img width="276" height="537" alt="image" src="https://github.com/user-attachments/assets/3c86c966-e716-4f93-8155-dfa7f2c32d97" />

You can see in 'files' the result yourself. As you can see, statistics based on given input data are generated.

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

