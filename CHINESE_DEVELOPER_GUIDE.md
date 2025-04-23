# 高级计算器开发者指南

## 项目概述
本项目是一个使用Free Pascal和Lazarus IDE开发的高级计算器应用程序。它提供了基本的算术运算和高级科学函数，包括三角函数、对数函数等，并具有内存功能和历史记录功能。

## 开发环境
- **编程语言**: Free Pascal
- **开发工具**: Lazarus IDE
- **目标平台**: Windows

## 项目结构

### 主要文件
- **project1.lpr**: 主项目文件，包含应用程序入口点
- **unit1.pas**: 主单元文件，包含所有业务逻辑和UI事件处理
- **unit1.lfm**: 窗体设计文件，定义UI布局
- **project1.ico**: 应用程序图标
- **project1.res**: 资源文件

### 类结构
项目主要基于`TForm1`类，该类继承自Lazarus的`TForm`类，并包含以下主要组件：

#### 私有字段
- `FMemory`: Double - 存储内存操作的值
- `FHistory`: TStringList - 存储计算历史
- `FCurrentOp`: String - 当前操作符
- `FOperand`: Double - 当前操作数
- `FIsNewInput`: Boolean - 标记是否开始新输入
- `FIsDegree`: Boolean - 标记是否使用角度制

#### 私有方法
- `AddToHistory`: 添加计算到历史记录
- `UpdateDisplay`: 更新显示内容
- 各种按钮点击事件处理方法

## 代码实现详解

### 初始化和清理

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  try
    // 初始化变量
    FMemory := 0;
    FCurrentOp := '';
    FOperand := 0;
    FIsNewInput := True;
    FIsDegree := True;
    
    // 初始化对象
    FHistory := TStringList.Create;
    
    // 初始化界面
    EditDisplay.Text := '0';
    LabelHistory.Caption := '';
    
    // 设置初始窗口高度
    Form1.Height := 490;
    
    // 设置按钮事件处理程序
    // ...
  except
    on E: Exception do
    begin
      ShowMessage('初始化错误: ' + E.Message);
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  try
    // 释放资源
    if Assigned(FHistory) then
    begin
      FHistory.Clear;
      FHistory.Free;
      FHistory := nil;
    end;
  except
    on E: Exception do
    begin
      // 静默处理异常
    end;
  end;
end;
```

### 数字输入处理

```pascal
procedure TForm1.ButtonNumClick(Sender: TObject);
begin
  try
    if not Assigned(Sender) then Exit;
    
    if FIsNewInput then
      EditDisplay.Text := '';
      
    EditDisplay.Text := EditDisplay.Text + (Sender as TButton).Caption;
    FIsNewInput := False;
  except
    on E: Exception do
    begin
      // 静默处理异常
    end;
  end;
end;
```

### 运算符处理

```pascal
procedure TForm1.ButtonOpClick(Sender: TObject);
begin
  try
    if not Assigned(Sender) then Exit;
    
    FOperand := StrToFloatDef(EditDisplay.Text, 0);
    FCurrentOp := (Sender as TButton).Caption;
    FIsNewInput := True;
  except
    on E: Exception do
    begin
      ShowMessage('操作错误: ' + E.Message);
    end;
  end;
end;
```

### 计算结果

```pascal
procedure TForm1.ButtonEqClick(Sender: TObject);
var
  value, result: Double;
begin
  try
    value := StrToFloatDef(EditDisplay.Text, 0);
    result := 0;
    
    if FCurrentOp = '+' then 
      result := FOperand + value
    else if FCurrentOp = '-' then 
      result := FOperand - value
    else if FCurrentOp = '×' then 
      result := FOperand * value
    else if FCurrentOp = '÷' then
    begin
      if value = 0 then
      begin
        ShowMessage('除数不能为0!');
        Exit;
      end
      else
        result := FOperand / value;
    end
    else if FCurrentOp = 'xʸ' then 
    begin
      // 检查幂运算是否可能导致溢出
      // ...
      result := Power(FOperand, value);
    end;
    
    // 检查结果是否有效
    if IsNan(result) or IsInfinite(result) then
    begin
      ShowMessage('计算结果无效!');
      Exit;
    end;
    
    UpdateDisplay(FloatToStr(result));
    AddToHistory(FloatToStr(FOperand) + ' ' + FCurrentOp + ' ' + FloatToStr(value) + ' = ' + FloatToStr(result));
    FIsNewInput := True;
  except
    on E: Exception do
    begin
      ShowMessage('计算错误: ' + E.Message);
      UpdateDisplay('0');
      FIsNewInput := True;
    end;
  end;
end;
```

## 错误处理策略

本项目采用了全面的错误处理策略，主要包括：

1. **输入验证**：使用`TryStrToFloat`函数验证用户输入是否为有效数字
2. **域检查**：对特殊函数（如对数、平方根）进行域检查
3. **结果验证**：使用`IsNan`和`IsInfinite`函数检查计算结果是否有效
4. **异常捕获**：所有关键方法都包含try-except块，捕获并处理可能的异常
5. **用户反馈**：通过`ShowMessage`函数向用户提供明确的错误信息

示例：
```pascal
procedure TForm1.ButtonSqrtClick(Sender: TObject);
var
  value, result: Double;
  validNumber: Boolean;
begin
  try
    // 先检查输入是否为有效数字
    validNumber := TryStrToFloat(EditDisplay.Text, value);
    if not validNumber then
    begin
      ShowMessage('请输入有效数字!');
      UpdateDisplay('0');
      FIsNewInput := True;
      Exit;
    end;
    
    if value < 0 then
    begin
      ShowMessage('负数不能开平方根!');
      Exit;
    end;
    
    result := Sqrt(value);
    
    // 检查结果是否有效
    if IsNan(result) or IsInfinite(result) then
    begin
      ShowMessage('计算结果无效!');
      Exit;
    end;
    
    UpdateDisplay(FloatToStr(result));
    AddToHistory('√(' + FloatToStr(value) + ') = ' + FloatToStr(result));
    FIsNewInput := True;
  except
    on E: Exception do
    begin
      ShowMessage('计算错误: ' + E.Message);
      UpdateDisplay('0');
      FIsNewInput := True;
    end;
  end;
end;
```

## 历史记录管理

历史记录使用`TStringList`对象管理，主要实现如下：

```pascal
procedure TForm1.AddToHistory(const S: string);
begin
  try
    // 安全检查
    if not Assigned(FHistory) then
    begin
      FHistory := TStringList.Create;
    end;
    
    // 添加到历史记录
    FHistory.Add(S);
    
    // 更新标签
    if Assigned(LabelHistory) then
      LabelHistory.Caption := S;
    
    // 更新历史记录列表
    if Assigned(ListHistory) and Assigned(ListHistory.Items) then
    begin
      // 清除"暂无历史记录"提示
      if (ListHistory.Items.Count = 1) and (ListHistory.Items[0] = '暂无历史记录') then
        ListHistory.Items.Clear;
        
      // 添加新记录
      ListHistory.Items.Insert(0, S);
      
      // 限制历史记录数量，避免过多
      if ListHistory.Items.Count > 50 then
        ListHistory.Items.Delete(ListHistory.Items.Count - 1);
    end;
  except
    on E: Exception do
    begin
      // 静默处理异常，避免弹出错误框
    end;
  end;
end;

procedure TForm1.ButtonHistoryClick(Sender: TObject);
begin
  try
    if Assigned(ListHistory) then
    begin
      // 如果历史记录为空，添加提示
      if (ListHistory.Items.Count = 0) and not ListHistory.Visible then
      begin
        ListHistory.Items.Add('暂无历史记录');
      end;

      // 切换历史记录列表的可见性
      ListHistory.Visible := not ListHistory.Visible;
      
      // 如果显示历史记录，调整窗口高度
      if ListHistory.Visible then
        Form1.Height := 490 + 120
      else
        Form1.Height := 490;
    end;
  except
    on E: Exception do
    begin
      // 静默处理异常
    end;
  end;
end;
```

## 内存管理

内存功能通过单一方法`ButtonMemoryClick`实现，根据不同的按钮执行不同的操作：

```pascal
procedure TForm1.ButtonMemoryClick(Sender: TObject);
var
  btn: TButton;
  value: Double;
begin
  try
    if not Assigned(Sender) then Exit;
    
    btn := Sender as TButton;
    value := StrToFloatDef(EditDisplay.Text, 0);
    
    if btn = ButtonMC then
    begin
      FMemory := 0;
      ShowMessage('内存已清除');
    end
    else if btn = ButtonMR then
    begin
      UpdateDisplay(FloatToStr(FMemory));
      FIsNewInput := True;
    end
    else if btn = ButtonMS then
    begin
      FMemory := value;
      ShowMessage('数值已存入内存: ' + FloatToStr(FMemory));
    end
    else if btn = ButtonMPlus then
    begin
      FMemory := FMemory + value;
      ShowMessage('内存值更新为: ' + FloatToStr(FMemory));
    end
    else if btn = ButtonMMinus then
    begin
      FMemory := FMemory - value;
      ShowMessage('内存值更新为: ' + FloatToStr(FMemory));
    end;
  except
    on E: Exception do
    begin
      // 静默处理异常
    end;
  end;
end;
```

## 编译和部署

### 编译步骤
1. 在Lazarus IDE中打开项目文件 `project1.lpi`
2. 选择 "运行" > "编译" 或按 F9 键编译项目
3. 编译成功后，可执行文件 `project1.exe` 将在项目目录中生成

### 部署注意事项
- 应用程序是独立的可执行文件，不需要额外的DLL或配置文件
- 可以直接复制 `project1.exe` 到目标计算机运行
- 应用程序需要Windows环境运行

## 扩展和维护建议

### 可能的扩展
1. **添加更多科学函数**：如双曲函数、统计函数等
2. **添加编程计算功能**：如位运算、进制转换等
3. **添加图形绘制功能**：可视化函数图像
4. **添加单位转换功能**：长度、重量、温度等单位转换
5. **添加历史记录保存功能**：将历史记录保存到文件

### 维护建议
1. **代码重构**：将相似功能的代码提取为通用方法
2. **增强错误处理**：添加更详细的错误日志
3. **优化UI响应**：改进用户界面的响应速度
4. **添加单元测试**：为关键功能添加自动化测试
5. **国际化支持**：添加多语言支持
