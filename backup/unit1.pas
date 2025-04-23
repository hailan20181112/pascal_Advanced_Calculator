unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Math, fpexprpars, Buttons;

type
  { TForm1 }
  TForm1 = class(TForm)
    EditDisplay: TEdit;
    LabelHistory: TLabel;
    ButtonMC, ButtonMR, ButtonMS, ButtonMPlus, ButtonMMinus: TBitBtn;
    ButtonSqrt, ButtonAbs, ButtonPow, ButtonFact, ButtonXSq: TBitBtn;
    ButtonSin, ButtonCos, ButtonTan, ButtonLog, ButtonLn, ButtonInv, ButtonExp: TBitBtn;
    ButtonSign, ButtonPercent, ButtonAC, ButtonCE: TBitBtn;
    Button7, Button8, Button9, ButtonDiv: TBitBtn;
    Button4, Button5, Button6, ButtonMul: TBitBtn;
    Button1, Button2, Button3, ButtonSub: TBitBtn;
    Button00, Button000, Button0, ButtonDot, ButtonAdd: TBitBtn;
    ButtonBack, ButtonEq: TBitBtn;
    ButtonDegRad, ButtonHistory, ButtonClearHistory: TBitBtn;
    ListHistory: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonNumClick(Sender: TObject);
    procedure ButtonOpClick(Sender: TObject);
    procedure ButtonEqClick(Sender: TObject);
    procedure ButtonACClick(Sender: TObject);
    procedure ButtonCEClick(Sender: TObject);
    procedure ButtonBackClick(Sender: TObject);
    procedure ButtonDotClick(Sender: TObject);
    procedure ButtonSignClick(Sender: TObject);
    procedure ButtonSqrtClick(Sender: TObject);
    procedure ButtonAbsClick(Sender: TObject);
    procedure ButtonPowClick(Sender: TObject);
    procedure ButtonXSqClick(Sender: TObject);
    procedure ButtonFactClick(Sender: TObject);
    procedure ButtonSinClick(Sender: TObject);
    procedure ButtonCosClick(Sender: TObject);
    procedure ButtonTanClick(Sender: TObject);
    procedure ButtonLogClick(Sender: TObject);
    procedure ButtonLnClick(Sender: TObject);
    procedure ButtonInvClick(Sender: TObject);
    procedure ButtonExpClick(Sender: TObject);
    procedure ButtonPercentClick(Sender: TObject);
    procedure ButtonDegRadClick(Sender: TObject);
    procedure ButtonHistoryClick(Sender: TObject);
    procedure ButtonClearHistoryClick(Sender: TObject);
    procedure ButtonMemoryClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMemory: Double;
    FHistory: TStringList;
    FCurrentOp: string;
    FOperand: Double;
    FIsNewInput: Boolean;
    FIsDegree: Boolean;
    procedure AddToHistory(const S: string);
    procedure UpdateDisplay(const S: string);
    function ValidateNumericInput(out Value: Double): Boolean;
    function CheckResultValidity(const AResult: Double): Boolean;
    procedure SetButtonColor(Button: TBitBtn; AColor: TColor);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

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

    // 设置窗口背景颜色
    Form1.Color := $FFFFCC; // 浅黄色背景

    // 设置显示区域样式
    EditDisplay.BorderStyle := bsNone;
    EditDisplay.Color := clWhite;

    // 设置按钮颜色
    // 内存按钮 - 浅绿色
    SetButtonColor(ButtonMC, $CCFFCC);
    SetButtonColor(ButtonMR, $CCFFCC);
    SetButtonColor(ButtonMS, $CCFFCC);
    SetButtonColor(ButtonMPlus, $CCFFCC);
    SetButtonColor(ButtonMMinus, $CCFFCC);

    // 数学/科学函数按钮 - 浅蓝色
    SetButtonColor(ButtonSqrt, $CCE5FF);
    SetButtonColor(ButtonAbs, $CCE5FF);
    SetButtonColor(ButtonPow, $CCE5FF);
    SetButtonColor(ButtonFact, $CCE5FF);
    SetButtonColor(ButtonXSq, $CCE5FF);
    SetButtonColor(ButtonSin, $CCE5FF);
    SetButtonColor(ButtonCos, $CCE5FF);
    SetButtonColor(ButtonTan, $CCE5FF);
    SetButtonColor(ButtonLog, $CCE5FF);
    SetButtonColor(ButtonLn, $CCE5FF);
    SetButtonColor(ButtonInv, $CCE5FF);
    SetButtonColor(ButtonExp, $CCE5FF);
    SetButtonColor(ButtonSign, $CCE5FF);
    SetButtonColor(ButtonPercent, $CCE5FF);

    // 数字按钮 - 白色
    SetButtonColor(Button0, clWhite);
    SetButtonColor(Button1, clWhite);
    SetButtonColor(Button2, clWhite);
    SetButtonColor(Button3, clWhite);
    SetButtonColor(Button4, clWhite);
    SetButtonColor(Button5, clWhite);
    SetButtonColor(Button6, clWhite);
    SetButtonColor(Button7, clWhite);
    SetButtonColor(Button8, clWhite);
    SetButtonColor(Button9, clWhite);
    SetButtonColor(Button00, clWhite);
    SetButtonColor(Button000, clWhite);
    SetButtonColor(ButtonDot, clWhite);

    // 运算符/主操作按钮 - 橙色
    SetButtonColor(ButtonAdd, $FFB366); // +
    SetButtonColor(ButtonSub, $FFB366); // -
    SetButtonColor(ButtonMul, $FFB366); // ×
    SetButtonColor(ButtonDiv, $FFB366); // ÷
    SetButtonColor(ButtonEq, $FF8000); // = 深橙色
    SetButtonColor(ButtonAC, $FFB366); // AC
    SetButtonColor(ButtonCE, $FFB366); // CE
    SetButtonColor(ButtonBack, $FFB366); // Back

    // 角度/历史相关按钮 - 浅紫色
    SetButtonColor(ButtonDegRad, $E0CCFF);
    SetButtonColor(ButtonHistory, $E0CCFF);
    SetButtonColor(ButtonClearHistory, $E0CCFF);

    // 设置初始窗口高度
    Form1.Height := 655;

  // 设置数字按钮事件
  Button0.OnClick := @ButtonNumClick;
  Button1.OnClick := @ButtonNumClick;
  Button2.OnClick := @ButtonNumClick;
  Button3.OnClick := @ButtonNumClick;
  Button4.OnClick := @ButtonNumClick;
  Button5.OnClick := @ButtonNumClick;
  Button6.OnClick := @ButtonNumClick;
  Button7.OnClick := @ButtonNumClick;
  Button8.OnClick := @ButtonNumClick;
  Button9.OnClick := @ButtonNumClick;
  Button00.OnClick := @ButtonNumClick;
  Button000.OnClick := @ButtonNumClick;

  // 设置运算符按钮事件
  ButtonAdd.OnClick := @ButtonOpClick;
  ButtonSub.OnClick := @ButtonOpClick;
  ButtonMul.OnClick := @ButtonOpClick;
  ButtonDiv.OnClick := @ButtonOpClick;

  // 设置功能按钮事件
  ButtonEq.OnClick := @ButtonEqClick;
  ButtonDot.OnClick := @ButtonDotClick;
  ButtonBack.OnClick := @ButtonBackClick;
  ButtonAC.OnClick := @ButtonACClick;
  ButtonCE.OnClick := @ButtonCEClick;

  // 设置科学函数按钮事件
  ButtonSqrt.OnClick := @ButtonSqrtClick;
  ButtonAbs.OnClick := @ButtonAbsClick;
  ButtonPow.OnClick := @ButtonPowClick;
  ButtonFact.OnClick := @ButtonFactClick;
  ButtonXSq.OnClick := @ButtonXSqClick;
  ButtonSin.OnClick := @ButtonSinClick;
  ButtonCos.OnClick := @ButtonCosClick;
  ButtonTan.OnClick := @ButtonTanClick;
  ButtonLog.OnClick := @ButtonLogClick;
  ButtonLn.OnClick := @ButtonLnClick;
  ButtonInv.OnClick := @ButtonInvClick;
  ButtonExp.OnClick := @ButtonExpClick;
  ButtonSign.OnClick := @ButtonSignClick;
  ButtonPercent.OnClick := @ButtonPercentClick;

  // 设置内存按钮事件
  ButtonMC.OnClick := @ButtonMemoryClick;
  ButtonMR.OnClick := @ButtonMemoryClick;
  ButtonMS.OnClick := @ButtonMemoryClick;
  ButtonMPlus.OnClick := @ButtonMemoryClick;
  ButtonMMinus.OnClick := @ButtonMemoryClick;

  // 设置其他按钮事件
  ButtonDegRad.OnClick := @ButtonDegRadClick;
  ButtonHistory.OnClick := @ButtonHistoryClick;
  ButtonClearHistory.OnClick := @ButtonClearHistoryClick;

  // 设置角度/弧度按钮初始状态
  ButtonDegRad.Caption := '角度制';

  // 设置历史记录列表
  ListHistory.Visible := False;

  except
    on E: Exception do
    begin
      ShowMessage('初始化错误: ' + E.Message);
    end;
  end;
end;

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
      // 清除“暂无历史记录”提示
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

procedure TForm1.UpdateDisplay(const S: string);
begin
  try
    if Assigned(EditDisplay) then
      EditDisplay.Text := S;
  except
    on E: Exception do
    begin
      ShowMessage('显示更新错误: ' + E.Message);
    end;
  end;
end;

function TForm1.ValidateNumericInput(out Value: Double): Boolean;
begin
  Result := False;
  try
    if not Assigned(EditDisplay) then Exit;

    // 检查输入是否为有效数字
    if not TryStrToFloat(EditDisplay.Text, Value) then
    begin
      ShowMessage('请输入有效数字!');
      UpdateDisplay('0');
      FIsNewInput := True;
      Exit;
    end;

    Result := True;
  except
    on E: Exception do
    begin
      ShowMessage('输入验证错误: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TForm1.CheckResultValidity(const AResult: Double): Boolean;
begin
  // 检查结果是否有效
  if IsNan(AResult) or IsInfinite(AResult) then
  begin
    ShowMessage('计算结果无效!');
    Exit(False);
  end;

  Exit(True);
end;

procedure TForm1.SetButtonColor(Button: TBitBtn; AColor: TColor);
begin
  // 设置按钮背景色
  Button.Color := AColor;
  // 设置按钮字体颜色
  if AColor = clMoneyGreen then
    Button.Font.Color := clRed
  else if AColor = clSkyBlue then
    Button.Font.Color := clBlue
  else if AColor = clSilver then
    Button.Font.Color := clGreen
  else
    Button.Font.Color := clBlack;

  // 设置按钮间距
  Button.BorderSpacing.Around := 3;
end;

procedure TForm1.ButtonNumClick(Sender: TObject);
var
  buttonCaption: string;
begin
  try
    if not Assigned(Sender) then Exit;

    buttonCaption := (Sender as TBitBtn).Caption;

    // 如果是新输入或当前显示为'0'，则替换而不是追加
    if FIsNewInput or (EditDisplay.Text = '0') then
      EditDisplay.Text := buttonCaption
    else
      EditDisplay.Text := EditDisplay.Text + buttonCaption;

    FIsNewInput := False;
  except
    on E: Exception do
    begin
      ShowMessage('输入错误: ' + E.Message);
    end;
  end;
end;

procedure TForm1.ButtonOpClick(Sender: TObject);
begin
  try
    if not Assigned(Sender) then Exit;

    FOperand := StrToFloatDef(EditDisplay.Text, 0);
    FCurrentOp := (Sender as TBitBtn).Caption;
    FIsNewInput := True;
  except
    on E: Exception do
    begin
      ShowMessage('操作错误: ' + E.Message);
    end;
  end;
end;

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
      if (FOperand = 0) and (value < 0) then
      begin
        ShowMessage('0的负数次方未定义!');
        Exit;
      end
      else if (Abs(FOperand) > 1000) or (Abs(value) > 100) then
      begin
        ShowMessage('数值过大，可能导致溢出!');
        Exit;
      end
      else
        result := Power(FOperand, value);
    end;

    // 使用通用方法检查结果有效性
    if not CheckResultValidity(result) then Exit;

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

procedure TForm1.ButtonACClick(Sender: TObject);
begin
  try
    if Assigned(EditDisplay) then
      EditDisplay.Text := '0';

    if Assigned(LabelHistory) then
      LabelHistory.Caption := '';

    FCurrentOp := '';
    FOperand := 0;
    FIsNewInput := True;
  except
    on E: Exception do
    begin
      // 静默处理异常
    end;
  end;
end;

procedure TForm1.ButtonCEClick(Sender: TObject);
begin
  try
    if Assigned(EditDisplay) then
      EditDisplay.Text := '0';

    FIsNewInput := True;
  except
    on E: Exception do
    begin
      // 静默处理异常
    end;
  end;
end;

procedure TForm1.ButtonBackClick(Sender: TObject);
begin
  try
    if not Assigned(EditDisplay) then Exit;

    if Length(EditDisplay.Text) > 0 then
      EditDisplay.Text := Copy(EditDisplay.Text, 1, Length(EditDisplay.Text) - 1);

    if EditDisplay.Text = '' then
      EditDisplay.Text := '0';
  except
    on E: Exception do
    begin
      // 静默处理异常
    end;
  end;
end;

procedure TForm1.ButtonDotClick(Sender: TObject);
begin
  try
    if not Assigned(EditDisplay) then Exit;

    // 如果是新输入，先设置为'0'
    if FIsNewInput then
    begin
      EditDisplay.Text := '0';
      FIsNewInput := False;
    end;

    // 确保只有一个小数点
    if Pos('.', EditDisplay.Text) = 0 then
      EditDisplay.Text := EditDisplay.Text + '.';
  except
    on E: Exception do
    begin
      ShowMessage('小数点输入错误: ' + E.Message);
    end;
  end;
end;

procedure TForm1.ButtonSignClick(Sender: TObject);
var
  value: Double;
  validNumber: Boolean;
begin
  try
    if not Assigned(EditDisplay) then Exit;

    // 检查输入是否为有效数字
    validNumber := TryStrToFloat(EditDisplay.Text, value);
    if not validNumber then
    begin
      ShowMessage('请输入有效数字!');
      Exit;
    end;

    EditDisplay.Text := FloatToStr(-value);
  except
    on E: Exception do
    begin
      ShowMessage('符号更改错误: ' + E.Message);
    end;
  end;
end;

procedure TForm1.ButtonSqrtClick(Sender: TObject);
var
  value, result: Double;
begin
  try
    // 使用通用方法验证输入
    if not ValidateNumericInput(value) then Exit;

    if value < 0 then
    begin
      ShowMessage('负数不能开平方根!');
      Exit;
    end;

    result := Sqrt(value);

    // 使用通用方法检查结果有效性
    if not CheckResultValidity(result) then Exit;

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

procedure TForm1.ButtonAbsClick(Sender: TObject);
var
  value, result: Double;
begin
  try
    // 使用通用方法验证输入
    if not ValidateNumericInput(value) then Exit;

    result := Abs(value);

    // 使用通用方法检查结果有效性
    if not CheckResultValidity(result) then Exit;

    UpdateDisplay(FloatToStr(result));
    AddToHistory('|' + FloatToStr(value) + '| = ' + FloatToStr(result));
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

procedure TForm1.ButtonPowClick(Sender: TObject);
begin
  ButtonOpClick(Sender);
end;

procedure TForm1.ButtonXSqClick(Sender: TObject);
var
  value, result: Double;
begin
  try
    // 使用通用方法验证输入
    if not ValidateNumericInput(value) then Exit;

    // 检查平方运算是否可能导致溢出
    if Abs(value) > 1.0E+154 then
    begin
      ShowMessage('数值过大，可能导致溢出!');
      Exit;
    end;

    result := Sqr(value);

    // 使用通用方法检查结果有效性
    if not CheckResultValidity(result) then Exit;

    UpdateDisplay(FloatToStr(result));
    AddToHistory(FloatToStr(value) + '² = ' + FloatToStr(result));
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

procedure TForm1.ButtonFactClick(Sender: TObject);
var
  i, n: Integer;
  res: Double;
  tempValue: Double;
begin
  try
    // 使用通用方法验证输入
    if not ValidateNumericInput(tempValue) then Exit;

    // 检查数字是否为整数
    if Frac(tempValue) <> 0 then
    begin
      ShowMessage('阶乘只能计算整数!');
      Exit;
    end;

    // 安全转换为整数
    n := Trunc(tempValue);

    if n < 0 then
      ShowMessage('负数没有阶乘!')
    else if n > 12 then
      ShowMessage('数值过大，可能导致溢出！')
    else
    begin
      res := 1;
      for i := 2 to n do
        res := res * i;
      UpdateDisplay(FloatToStr(res));
      AddToHistory(IntToStr(n) + '! = ' + FloatToStr(res));
      FIsNewInput := True;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('计算错误: ' + E.Message);
      UpdateDisplay('0');
      FIsNewInput := True;
    end;
  end;
end;

procedure TForm1.ButtonSinClick(Sender: TObject);
var
  value, result: Double;
begin
  try
    // 使用通用方法验证输入
    if not ValidateNumericInput(value) then Exit;

    if FIsDegree then
      result := Sin(DegToRad(value))
    else
      result := Sin(value);

    // 使用通用方法检查结果有效性
    if not CheckResultValidity(result) then Exit;

    UpdateDisplay(FloatToStr(result));
    AddToHistory('sin(' + FloatToStr(value) + ') = ' + FloatToStr(result));
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

procedure TForm1.ButtonCosClick(Sender: TObject);
var
  value, result: Double;
begin
  try
    // 使用通用方法验证输入
    if not ValidateNumericInput(value) then Exit;

    if FIsDegree then
      result := Cos(DegToRad(value))
    else
      result := Cos(value);

    // 使用通用方法检查结果有效性
    if not CheckResultValidity(result) then Exit;

    UpdateDisplay(FloatToStr(result));
    AddToHistory('cos(' + FloatToStr(value) + ') = ' + FloatToStr(result));
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

procedure TForm1.ButtonTanClick(Sender: TObject);
var
  value, result: Double;
begin
  try
    // 使用通用方法验证输入
    if not ValidateNumericInput(value) then Exit;

    // 检查是否为切线的无效值
    if FIsDegree then
    begin
      // 在角度制下，检查是否为 90°, 270° 等的倍数
      if Abs(Cos(DegToRad(value))) < 1.0E-10 then
      begin
        ShowMessage('切线在该角度下无定义!');
        Exit;
      end;
      result := Tan(DegToRad(value));
    end
    else
    begin
      // 在弧度制下，检查是否为 π/2, 3π/2 等的倍数
      if Abs(Cos(value)) < 1.0E-10 then
      begin
        ShowMessage('切线在该角度下无定义!');
        Exit;
      end;
      result := Tan(value);
    end;

    // 使用通用方法检查结果有效性
    if not CheckResultValidity(result) then Exit;

    UpdateDisplay(FloatToStr(result));
    AddToHistory('tan(' + FloatToStr(value) + ') = ' + FloatToStr(result));
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

procedure TForm1.ButtonLogClick(Sender: TObject);
var
  value, result: Double;
begin
  try
    // 使用通用方法验证输入
    if not ValidateNumericInput(value) then Exit;

    if value <= 0 then
    begin
      ShowMessage('对数函数的参数必须大于0!');
      Exit;
    end;

    result := Log10(value);

    // 使用通用方法检查结果有效性
    if not CheckResultValidity(result) then Exit;

    UpdateDisplay(FloatToStr(result));
    AddToHistory('log(' + FloatToStr(value) + ') = ' + FloatToStr(result));
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

procedure TForm1.ButtonLnClick(Sender: TObject);
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

    if value <= 0 then
    begin
      ShowMessage('自然对数的参数必须大于0!');
      Exit;
    end;

    result := Ln(value);

    // 检查结果是否有效
    if IsNan(result) or IsInfinite(result) then
    begin
      ShowMessage('计算结果无效!');
      Exit;
    end;

    UpdateDisplay(FloatToStr(result));
    AddToHistory('ln(' + FloatToStr(value) + ') = ' + FloatToStr(result));
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

procedure TForm1.ButtonInvClick(Sender: TObject);
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

    if value = 0 then
    begin
      ShowMessage('除数不能为0!');
      Exit;
    end;

    result := 1 / value;

    // 检查结果是否有效
    if IsNan(result) or IsInfinite(result) then
    begin
      ShowMessage('计算结果无效!');
      Exit;
    end;

    UpdateDisplay(FloatToStr(result));
    AddToHistory('1/' + FloatToStr(value) + ' = ' + FloatToStr(result));
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

procedure TForm1.ButtonExpClick(Sender: TObject);
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

    // 检查指数运算是否可能导致溢出
    if value > 709 then
    begin
      ShowMessage('指数过大，可能导致溢出!');
      Exit;
    end;

    result := Exp(value);

    // 检查结果是否有效
    if IsNan(result) or IsInfinite(result) then
    begin
      ShowMessage('计算结果无效!');
      Exit;
    end;

    UpdateDisplay(FloatToStr(result));
    AddToHistory('e^' + FloatToStr(value) + ' = ' + FloatToStr(result));
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

procedure TForm1.ButtonPercentClick(Sender: TObject);
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

    result := value / 100;

    // 检查结果是否有效
    if IsNan(result) or IsInfinite(result) then
    begin
      ShowMessage('计算结果无效!');
      Exit;
    end;

    UpdateDisplay(FloatToStr(result));
    AddToHistory(FloatToStr(value) + '% = ' + FloatToStr(result));
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

procedure TForm1.ButtonDegRadClick(Sender: TObject);
begin
  FIsDegree := not FIsDegree;
  if FIsDegree then
    ButtonDegRad.Caption := '角度制'
  else
    ButtonDegRad.Caption := '弧度制';
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
        Form1.Height := 785 // 655 + 130 (历史记录区域高度)
      else
        Form1.Height := 655;
    end;
  except
    on E: Exception do
    begin
      // 静默处理异常
    end;
  end;
end;

procedure TForm1.ButtonClearHistoryClick(Sender: TObject);
begin
  try
    // 清除历史记录对象
    if Assigned(FHistory) then
    begin
      FHistory.Clear;
      ShowMessage('历史记录已清除');
    end;

    // 清除历史记录列表
    if Assigned(ListHistory) and Assigned(ListHistory.Items) then
    begin
      ListHistory.Items.Clear;
      ListHistory.Items.Add('暂无历史记录');
    end;

    // 清除历史记录标签
    if Assigned(LabelHistory) then
      LabelHistory.Caption := '';
  except
    on E: Exception do
    begin
      ShowMessage('清除历史记录错误: ' + E.Message);
    end;
  end;
end;

procedure TForm1.ButtonMemoryClick(Sender: TObject);
var
  btn: TBitBtn;
  value: Double;
begin
  try
    if not Assigned(Sender) then Exit;

    btn := Sender as TBitBtn;
    value := StrToFloatDef(EditDisplay.Text, 0);

    if btn = ButtonMC then
    begin
      FMemory := 0;
      ShowMessage('内存已清除');
    end
    else if btn = ButtonMR then
    begin
      UpdateDisplay(FloatToStr(FMemory));
      ShowMessage('内存值调用: ' + FloatToStr(FMemory));
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

end.