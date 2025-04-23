# 🧮 Advanced Calculator / 高级计算器

## 📖 English Documentation

### 🚀 Development Process
1. **Project Initialization**: Created Lazarus project and designed the main form.
2. **UI Design**: All buttons use TBitBtn, grouped and color-coded by function.
3. **Function Implementation**: Implemented arithmetic, scientific, memory, and history features.
4. **UI Optimization**: Switched from TButton to TBitBtn for better color support and appearance.
5. **Cross-Platform Compilation**: Configured for both Win32 (32-bit) and Win64 (64-bit) targets.
6. **Testing & Packaging**: Compiled, tested, and prepared for distribution.

### 🏗️ Build & Packaging
- **32-bit**: Project Options → Target OS: Win32, CPU: i386, then F9 to build.
- **64-bit**: Project Options → Target OS: Win64, CPU: x86_64, then F9 to build.
- Use Inno Setup or NSIS to make an installer if needed.

### 📁 Directory Structure
- `unit1.pas`, `unit1.lfm`: Main form logic and UI.
- `project1.lpi`, `project1.lpr`: Project files.
- `lib/`, `backup/`: Build output and backups.
- `imang/`: Icons and installer scripts.
- Documentation: `README.md`, `README_EN.md`, manuals.

### 🧹 Cleanup Recommendations
- Before release, you may delete `lib/`, `backup/`, `coverage/` folders.
- Only distribute `project1.exe`, `project1.ico`, and documentation.

### 🙏 Thanks
Thanks to the Lazarus/FPC community and all contributors.


### 📝 Project Overview
This project is an advanced calculator application developed using Free Pascal and Lazarus IDE. It provides a user-friendly interface with various mathematical functions, including basic arithmetic operations, trigonometric functions, logarithmic functions, and more.

### ✨ Features
- Basic arithmetic operations (addition, subtraction, multiplication, division)
- Scientific functions (sin, cos, tan, log, ln, exp, etc.)
- Memory functions (MC, MR, MS, M+, M-)
- History record viewing
- Angle mode switching (degrees/radians)
- Error handling and input validation

### 🗂️ Project Structure
- **project1.lpr**: Main project file
- **unit1.pas**: Main unit containing all the calculator logic and UI event handlers
- **unit1.lfm**: Form design file
- **project1.ico**: Application icon
- **project1.res**: Resource file

### ⚙️ Technical Details
- **Programming Language**: Free Pascal
- **Framework**: Lazarus
- **UI Components**: Standard Lazarus components (TButton, TEdit, TLabel, TListBox)
- **Error Handling**: Comprehensive error checking for all mathematical operations
- **Input Validation**: Validation for all user inputs to prevent crashes

### 💻 Installation and Usage
1. Open the project in Lazarus IDE
2. Compile the project (F9)
3. Run the application (F9 or Ctrl+F9)

### Key Functions
- **ButtonNumClick**: Handles numeric button clicks
- **ButtonOpClick**: Handles operation button clicks
- **ButtonEqClick**: Calculates and displays the result
- **ButtonFactClick**: Calculates factorial
- **ButtonSinClick/ButtonCosClick/ButtonTanClick**: Calculates trigonometric functions
- **ButtonLogClick/ButtonLnClick**: Calculates logarithmic functions
- **ButtonExpClick**: Calculates exponential function
- **ButtonHistoryClick**: Shows/hides calculation history

### Error Handling
The application includes comprehensive error handling for:
- Division by zero
- Invalid logarithm inputs (negative or zero)
- Factorial of negative numbers
- Square root of negative numbers
- Overflow prevention for large calculations
- Invalid inputs (non-numeric characters)

---

## 📖 中文文档

### 🚀 开发全过程
1. **项目初始化**：新建Lazarus工程，搭建主界面。
2. **界面设计**：所有按钮采用TBitBtn，按功能分组并着色。
3. **功能实现**：实现所有基础与科学运算、内存、历史等功能。
4. **UI优化**：将TButton全部替换为TBitBtn，完善按钮配色与布局。
5. **跨平台编译**：配置支持Win32与Win64目标。
6. **打包与测试**：多次编译测试，准备发布版。

### 🏗️ 编译与打包说明
- 32位：项目选项→目标OS选Win32，CPU选i386，F9编译。
- 64位：项目选项→目标OS选Win64，CPU选x86_64，F9编译。
- 可用Inno Setup等工具生成安装包。

### 📁 目录结构
- `unit1.pas`、`unit1.lfm`：主窗体逻辑与界面
- `project1.lpi`、`project1.lpr`：工程文件
- `lib/`、`backup/`：编译输出与备份
- `imang/`：图标和安装脚本
- 文档：`README.md`、`README_EN.md`、用户手册等

### 🧹 文件清理建议
- 发布前可删除 `lib/`、`backup/`、`coverage/` 等临时目录
- 正式分发仅需 `project1.exe`、`project1.ico`、文档等

### 🙏 致谢
感谢所有开源社区与Lazarus团队的支持。


### 📝 项目概述
本项目是使用Free Pascal和Lazarus IDE开发的高级计算器应用程序。它提供了用户友好的界面和各种数学函数，包括基本算术运算、三角函数、对数函数等。

### ✨ 功能特点
- 基本算术运算（加、减、乘、除）
- 科学函数（正弦、余弦、正切、对数、自然对数、指数等）
- 内存功能（MC、MR、MS、M+、M-）
- 历史记录查看
- 角度模式切换（角度制/弧度制）
- 错误处理和输入验证

### 项目结构
- **project1.lpr**: 主项目文件
- **unit1.pas**: 包含所有计算器逻辑和UI事件处理程序的主单元
- **unit1.lfm**: 窗体设计文件
- **project1.ico**: 应用程序图标
- **project1.res**: 资源文件

### 技术细节
- **编程语言**: Free Pascal
- **框架**: Lazarus
- **UI组件**: 标准Lazarus组件（TButton、TEdit、TLabel、TListBox）
- **错误处理**: 所有数学运算的全面错误检查
- **输入验证**: 对所有用户输入进行验证，防止程序崩溃

### 安装和使用
1. 在Lazarus IDE中打开项目
2. 编译项目（F9）
3. 运行应用程序（F9或Ctrl+F9）

### 关键函数
- **ButtonNumClick**: 处理数字按钮点击
- **ButtonOpClick**: 处理运算符按钮点击
- **ButtonEqClick**: 计算并显示结果
- **ButtonFactClick**: 计算阶乘
- **ButtonSinClick/ButtonCosClick/ButtonTanClick**: 计算三角函数
- **ButtonLogClick/ButtonLnClick**: 计算对数函数
- **ButtonExpClick**: 计算指数函数
- **ButtonHistoryClick**: 显示/隐藏计算历史记录

### 错误处理
应用程序包含全面的错误处理，包括：
- 除零错误
- 无效的对数输入（负数或零）
- 负数的阶乘
- 负数的平方根
- 大型计算的溢出防止
- 无效输入（非数字字符）
