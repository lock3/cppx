
This is how I've been discovering and running all any lit tests.
You can use a relative path to the folder, but I chose not to. I recommend
sending your output to a JSON file otherwise you're not going to know what went
wrong.
```Bash
~/projects/cppx/clang-cppx/build_sys$ bin/./llvm-lit /home/brian/projects/cppx/clang-cppx/clang/test/Gold -o something.json
```
Currently I have one test building and executing. The clang-gold compiler needs
to be build before this is run.