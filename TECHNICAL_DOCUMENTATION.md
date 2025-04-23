# Technical Documentation / 技术文档

## English Documentation

### Code Structure

#### Main Form (TForm1)
The main form is the central component of the application, containing all UI elements and handling all user interactions.

#### Class Variables
- `FMemory`: Stores the value for memory operations (MC, MR, MS, M+, M-)
- `FHistory`: TStringList object that stores calculation history
- `FCurrentOp`: Current operation being performed
- `FOperand`: First operand in a binary operation
- `FIsNewInput`: Flag indicating whether the next digit should start a new input
- `FIsDegree`: Flag indicating whether angle measurements are in degrees or radians

#### UI Components
- `EditDisplay`: Main display showing current input or result
- `LabelHistory`: Label showing the last calculation
- `ListHistory`: ListBox showing calculation history
- Numeric buttons (0-9, 00, .)
- Operation buttons (+, -, ×, ÷)
- Function buttons (sin, cos, tan, log, ln, etc.)
- Memory buttons (MC, MR, MS, M+, M-)
- Control buttons (AC, CE, ⌫)

#### Key Methods

##### Initialization and Cleanup
- `FormCreate`: Initializes variables, objects, and UI
- `FormDestroy`: Frees allocated resources

##### Input Handling
- `ButtonNumClick`: Handles numeric button clicks
- `ButtonDotClick`: Handles decimal point button click
- `ButtonBackClick`: Handles backspace button click
- `ButtonSignClick`: Handles sign change button click

##### Operation Handling
- `ButtonOpClick`: Handles operation button clicks
- `ButtonEqClick`: Calculates and displays the result

##### Function Handling
- `ButtonSqrtClick`: Calculates square root
- `ButtonXSqClick`: Calculates square
- `ButtonFactClick`: Calculates factorial
- `ButtonSinClick`: Calculates sine
- `ButtonCosClick`: Calculates cosine
- `ButtonTanClick`: Calculates tangent
- `ButtonLogClick`: Calculates base-10 logarithm
- `ButtonLnClick`: Calculates natural logarithm
- `ButtonExpClick`: Calculates exponential function
- `ButtonAbsClick`: Calculates absolute value
- `ButtonInvClick`: Calculates reciprocal
- `ButtonPercentClick`: Calculates percentage

##### Memory Handling
- `ButtonMemoryClick`: Handles all memory operations (MC, MR, MS, M+, M-)

##### UI Management
- `ButtonACClick`: Clears all input and memory
- `ButtonCEClick`: Clears current input
- `ButtonDegRadClick`: Toggles between degrees and radians
- `ButtonHistoryClick`: Shows/hides calculation history
- `AddToHistory`: Adds a calculation to the history
- `UpdateDisplay`: Updates the display with a new value

### Implementation Details

#### Error Handling
All mathematical operations include comprehensive error checking:
- Input validation using `TryStrToFloat` to ensure valid numeric input
- Range checking to prevent overflow/underflow
- Domain checking for functions with restricted domains (e.g., logarithms, square roots)
- Result validation using `IsNan` and `IsInfinite` to detect invalid results

#### Memory Management
- Memory is initialized to 0 at startup
- Memory operations are handled through a single method (`ButtonMemoryClick`)
- Memory value is preserved between calculations

#### History Management
- History is stored in a TStringList object
- New calculations are added to the top of the history list
- History list is limited to 50 entries to prevent excessive memory usage
- History can be viewed/hidden via a toggle button

#### UI Design
- Compact layout with minimal spacing
- Color-coded buttons for better usability
- Dynamic window resizing when showing/hiding history
- Clear error messages for invalid operations

---

## 中文文档

### 代码结构

#### 主窗体 (TForm1)
主窗体是应用程序的核心组件，包含所有UI元素并处理所有用户交互。

#### 类变量
- `FMemory`: 存储内存操作的值 (MC, MR, MS, M+, M-)
- `FHistory`: 存储计算历史的TStringList对象
- `FCurrentOp`: 当前执行的操作
- `FOperand`: 二元操作中的第一个操作数
- `FIsNewInput`: 指示下一个数字是否应该开始新输入的标志
- `FIsDegree`: 指示角度测量是否为角度制的标志

#### UI组件
- `EditDisplay`: 显示当前输入或结果的主显示区
- `LabelHistory`: 显示最后一次计算的标签
- `ListHistory`: 显示计算历史的列表框
- 数字按钮 (0-9, 00, .)
- 运算按钮 (+, -, ×, ÷)
- 函数按钮 (sin, cos, tan, log, ln等)
- 内存按钮 (MC, MR, MS, M+, M-)
- 控制按钮 (AC, CE, ⌫)

#### 关键方法

##### 初始化和清理
- `FormCreate`: 初始化变量、对象和UI
- `FormDestroy`: 释放分配的资源

##### 输入处理
- `ButtonNumClick`: 处理数字按钮点击
- `ButtonDotClick`: 处理小数点按钮点击
- `ButtonBackClick`: 处理退格按钮点击
- `ButtonSignClick`: 处理符号更改按钮点击

##### 运算处理
- `ButtonOpClick`: 处理运算按钮点击
- `ButtonEqClick`: 计算并显示结果

##### 函数处理
- `ButtonSqrtClick`: 计算平方根
- `ButtonXSqClick`: 计算平方
- `ButtonFactClick`: 计算阶乘
- `ButtonSinClick`: 计算正弦
- `ButtonCosClick`: 计算余弦
- `ButtonTanClick`: 计算正切
- `ButtonLogClick`: 计算以10为底的对数
- `ButtonLnClick`: 计算自然对数
- `ButtonExpClick`: 计算指数函数
- `ButtonAbsClick`: 计算绝对值
- `ButtonInvClick`: 计算倒数
- `ButtonPercentClick`: 计算百分比

##### 内存处理
- `ButtonMemoryClick`: 处理所有内存操作 (MC, MR, MS, M+, M-)

##### UI管理
- `ButtonACClick`: 清除所有输入和内存
- `ButtonCEClick`: 清除当前输入
- `ButtonDegRadClick`: 在角度制和弧度制之间切换
- `ButtonHistoryClick`: 显示/隐藏计算历史
- `AddToHistory`: 将计算添加到历史记录
- `UpdateDisplay`: 用新值更新显示

### 实现细节

#### 错误处理
所有数学运算都包含全面的错误检查：
- 使用`TryStrToFloat`进行输入验证，确保有效的数字输入
- 范围检查以防止溢出/下溢
- 对具有受限域的函数进行域检查（例如，对数、平方根）
- 使用`IsNan`和`IsInfinite`进行结果验证，检测无效结果

#### 内存管理
- 内存在启动时初始化为0
- 内存操作通过单一方法（`ButtonMemoryClick`）处理
- 计算之间保留内存值

#### 历史记录管理
- 历史记录存储在TStringList对象中
- 新计算添加到历史列表的顶部
- 历史列表限制为50个条目，以防止过度内存使用
- 可以通过切换按钮查看/隐藏历史记录

#### UI设计
- 紧凑布局，最小间距
- 按钮颜色编码，提高可用性
- 显示/隐藏历史记录时动态调整窗口大小
- 对无效操作提供清晰的错误消息
