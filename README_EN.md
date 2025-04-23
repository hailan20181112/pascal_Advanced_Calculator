# Advanced Calculator Project

## Overview
This project is a feature-rich calculator application developed using Lazarus (Free Pascal). It supports basic arithmetic, scientific functions, memory operations, and history tracking. The application is designed with a modern UI, supporting custom button colors for better usability.

## Features
- Basic arithmetic operations (+, -, ×, ÷)
- Scientific functions (sin, cos, tan, log, ln, sqrt, power, factorial, etc.)
- Memory operations (MC, MR, MS, M+, M-)
- History list and history clearing
- Custom button colors and categorized UI
- Support for both 32-bit and 64-bit Windows

## Development Process
1. **Project Initialization**: Created Lazarus project and designed the main form.
2. **UI Design**: Added TBitBtn buttons for all calculator functions, grouped and color-coded by function.
3. **Function Implementation**: Implemented all arithmetic and scientific operations, memory, and history features.
4. **UI Optimization**: Switched from TButton to TBitBtn for better color support, improved overall appearance.
5. **Cross-Platform Compilation**: Configured project for both Win32 (32-bit) and Win64 (64-bit) targets.
6. **Testing & Packaging**: Compiled and tested on Windows, prepared for distribution.

## How to Build
1. Open the project in Lazarus IDE.
2. To build for 32-bit: Set Target OS to Win32, CPU to i386.
3. To build for 64-bit: Set Target OS to Win64, CPU to x86_64.
4. Press F9 to build and run.

## Directory Structure
- `unit1.pas`, `unit1.lfm`: Main form logic and UI.
- `project1.lpi`, `project1.lpr`: Project files.
- `lib/`, `backup/`: Build output and backups.
- `imang/`: Icons and installer scripts.
- `README.md`, `README_EN.md`: Documentation.

## Cleanup Recommendations
- You may delete the `lib/`, `backup/`, `coverage/` folders before release.
- Only distribute `project1.exe`, `project1.ico`, and documentation.

## Author & License
- Author: [Your Name]
- License: MIT (or your choice)

---

## 中文说明
请参考 `CHINESE_DEVELOPER_GUIDE.md`、`CHINESE_USER_MANUAL.md` 等文件。
