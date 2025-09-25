# 🧪 ZincLang

ZincLang is a simple, experimental programming language built from scratch.  
It’s designed to be **lightweight, minimal, and fun to explore**, while giving hands-on experience with compilers, interpreters, and language design.

---

## ✨ Features
- 📦 Variables and constants  
- 🔁 Loops (`for`, `while`, `times`)  
- 🔀 Conditionals (`if`, `else`)  
- 🧩 Functions and scoping  
- 🔢 Basic types: integers, floats, strings , bools
- 🔢 derived types: Array
- 🖨️ Built-in `print` function  
- 🛠️ NASM x86-64 code generation (experimental)  

---

## 🚀 Getting Started

### 1. Clone the repo
```bash
git clone https://github.com/priyanshugaurav/zincLang.git
cd zincLang
```
### 2. Build
```bash
mkdir build && cd build
cmake ..
make
```
### 3. Run a program
```bash
./zinc ../examples/hello.zn
```
### 📂 Project Structure
```bash
zincLang/
├── src/          # Compiler source code
│   ├── lexer/    # Tokenizer
│   ├── parser/   # AST builder
│   ├── codegen/  # NASM backend
│   └── runtime/  # Built-in functions
├── examples/     # Example ZincLang programs
├── docs/         # Documentation
└── README.md     
```
### 📝 Example Code
```zinc
fn main() {
    var a = 10
    var b = 20
    print(a + b)
}
```
### Output:
```
30
```
<br>

---

### 📖 Documentation

Detailed documentation is available in the [docs/index.md](./docs/index.md)
 file.
This includes:

- Installation guide

- Language syntax

- Code examples

--- 

<br>

### 📖 Roadmap
 - Add arrays and dictionaries

 - Improve error messages

 - Optimizations in codegen

---

### 🤝 Contributing
Pull requests are welcome!
For major changes, please open an issue first to discuss what you’d like to change.