using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using OxyPlot;
using OxyPlot.Series;
using System.ComponentModel;
using System.DirectoryServices.ActiveDirectory;
using System.Runtime.CompilerServices;
using System.Text.Json.Nodes;
using Microsoft.FSharp.Core;
using Microsoft.FSharp.Collections;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using MathInterpreter;
using GUI.MVVM;
using OxyPlot.Axes;

namespace GUI
{
    public class MainViewModel : INotifyPropertyChanged
    {
        
        private readonly WindowService _windowService =  new WindowService();
        // Status TextBlock
        private string _statusMessage = string.Empty;
        private string _currentExpression = string.Empty;
        private string _inputExpression = string.Empty;
        private string _compilerOutput = string.Empty;
        private string _terminalOutput = string.Empty;
        private readonly string _compilerOutputDir = "out/";
        private string _selectedLanguage = "C";
        
        private float _xMinInput = 0;
        private float _xMaxInput = 0;
        private float _stepInput = 0;
        private bool _markerEnabled = false;
        private bool _scaleEnabled = false;
        
        public RelayCommand ExitCommand => new RelayCommand(_ => Exit());
        
        public RelayCommand HelpWindowShow => new RelayCommand(_ => ShowHelpWindow());
        public RelayCommand AboutWindowShow => new RelayCommand(_ => ShowAboutWindow());

        public RelayCommand InputEnter => new RelayCommand(_ => SubmitExpression());

        public RelayCommand PlotEnter => new RelayCommand(_ => RedrawExpression());

        public RelayCommand CompileEnter => new RelayCommand(_ => CompileExpression());
        public PlotModel MyModel { get; private set; }

        public ObservableCollection<SymbolTableEntry> SymbolTable { get; } = new ObservableCollection<SymbolTableEntry>();
        public ObservableCollection<string> Languages { get; } = new() { "C", "RISC-V" };
        
        public MainViewModel()
        {
            Func<double, double> myFun1 = (x) => 2 * x;
            Func<double, double> sinFunc = (x) => Math.Sin(x);
            this.MyModel = new PlotModel { Title = "sin(x)" };
            this.MyModel.Series.Add(new FunctionSeries(sinFunc, 0, 10, 0.1, "sin(x)"));
            
            LinearAxis xAxis = new LinearAxis
            {
                Position = AxisPosition.Bottom,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot,
                MajorGridlineColor = OxyColors.Gray,
                MinorGridlineColor = OxyColors.LightGray,
            };
            LinearAxis yAxis = new LinearAxis
            {
                Position = AxisPosition.Left,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot,
                MajorGridlineColor = OxyColors.Gray,
                MinorGridlineColor = OxyColors.LightGray,
            };
            
            MyModel.Axes.Add(xAxis);
            MyModel.Axes.Add(yAxis);

            xAxis.AxisChanged += OnAxisChanged;
            
            AppendToConsole(">> Welcome", false);
            XMinInput = 0;
            XMaxInput = 10;
            StepInput = 0.1f;
            CurrentExpression = "sin(x)";
        }
        
        
        public string StatusMessage
        {
            get => _statusMessage;
            set
            {
                if(_statusMessage == value)
                {
                    return;
                }
                _statusMessage = value;
                OnPropertyChanged();
            }
        }
        
        public string CompilerOutput
        {
            get => _compilerOutput;
            set
            {
                if(_compilerOutput == value)
                {
                    return;
                }
                _compilerOutput = value;
                OnPropertyChanged();
            }
        }
        
        public string SelectedLanguage
        {
            get => _selectedLanguage;
            set
            {
                if(_selectedLanguage == value)
                {
                    return;
                }
                _selectedLanguage = value;
                OnPropertyChanged();
            }
        }
        
        public string? TerminalOutput
        {
            get => _terminalOutput;
            set
            {
                if(_terminalOutput == value)
                {
                    return;
                }
                _terminalOutput = value;
                OnPropertyChanged();
            }
        }
        
        public float XMaxInput
        {
            get => _xMaxInput;
            set
            {
                if(_xMaxInput == value)
                {
                    return;
                }
                _xMaxInput = value;
                OnPropertyChanged();
            }
        }
        
        public float XMinInput
        {
            get => _xMinInput;
            set
            {
                if(_xMinInput == value)
                {
                    return;
                }
                _xMinInput = value;
                OnPropertyChanged();
            }
        }
        
        public float StepInput
        {
            get => _stepInput;
            set
            {
                if(_stepInput != value)
                {
                    _stepInput = value;
                    OnPropertyChanged();
                }
            }
        }

        public bool MarkerEnabled
        {
            get => _markerEnabled;
            set
            {
                if (_markerEnabled != value)
                {
                    _markerEnabled = value;
                    OnPropertyChanged();
                }
            }
        }
        
        public bool ScaleEnabled
        {
            get => _scaleEnabled;
            set
            {
                if (_scaleEnabled != value)
                {
                    _scaleEnabled = value;
                    OnPropertyChanged();
                }
            }
        }

        public string InputExpression
        {
            get => _inputExpression;
            set
            {
                if(_inputExpression != value)
                {
                    _inputExpression = value;
                    OnPropertyChanged();
                }
            }
        }
        
        public string CurrentExpression
        {
            get => _currentExpression;
            set
            {
                if(_currentExpression != value)
                {
                    _currentExpression = value;
                    OnPropertyChanged();
                }
            }
        }
        
        public event PropertyChangedEventHandler? PropertyChanged;
        private void OnPropertyChanged([CallerMemberName] string? propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
        
        private void OnAxisChanged(object? sender, AxisChangedEventArgs e)
        {
            if (sender is LinearAxis xAxis && ScaleEnabled)
            {
                double min = xAxis.ActualMinimum;
                double max = xAxis.ActualMaximum;

                PlotExpression(CurrentExpression, min, max);

                MyModel.InvalidatePlot(false);
            }
        }
        
        private void Exit()
        {
            Application.Current.Shutdown();
        }

        private void ShowHelpWindow()
        {
            var helpViewModel = new HelpViewModel();
            _windowService.ShowWindow(helpViewModel);
        }
        
        private void ShowAboutWindow()
        {
            var aboutViewModel = new AboutViewModel();
            _windowService.ShowWindow(aboutViewModel);
        }

        private void AppendToConsole(string str, bool marker)
        {
            StatusMessage += marker ? $">> { str }\n" : $"{ str }\n";
        }

        private void AppendExceptionToConsole(Exception ex)
        {
            AppendToConsole(ex.Message, false);
        }
        
        private void AppendExceptionToConsole(string ex)
        {
            AppendToConsole(ex, false);
        }

        private void RedrawExpression()
        {
            PlotExpression(InputExpression, XMinInput, XMaxInput);
            MyModel.InvalidatePlot(false);
        }

        private void PlotExpression(string expression, double xMin, double xMax)
        {
            if (expression == string.Empty) return;
            CurrentExpression = expression;
            try
            {
                string json = MathInterpreter.interpreter.evalPlot(expression, xMin, xMax, StepInput);
                JObject obj = JObject.Parse(json);
                string type = (string)obj["type"];

                if (type == "plot")
                {
                    double[] x = obj["x"].ToObject<double[]>();
                    double[] y = obj["y"].ToObject<double[]>();

                    MyModel.Title = expression;
                    MyModel.Series.Clear();
                    var series = new LineSeries { Title = expression };
                    //PlotModel model = new PlotModel { Title = $"{expression}" };
                    //LineSeries series = new LineSeries { Title = expression };
                    if (MarkerEnabled)
                    {
                        series.MarkerType = MarkerType.Circle;
                        series.MarkerSize = 3;
                        series.MarkerStroke = OxyColors.Black;
                    }

                    for (int i = 0; i < x.Length; i++)
                    {
                        series.Points.Add(new DataPoint(x[i], y[i]));
                    }

                    MyModel.Series.Add(series);
                    //MyModel.InvalidatePlot(true);
                    //OnPropertyChanged(nameof(MyModel));
                }
            }
            catch (Exception e)
            {
                AppendExceptionToConsole($"Plot failed: {e.Message}");
            }
        }
        private void SubmitExpression()
        {
            string expression = _inputExpression.ToString();
            AppendToConsole(expression, true);
            if (expression == string.Empty) return;
            try
            {
                var result = MathInterpreter.interpreter.evaluate(expression);
                
                string resultStr;

                if (result is MathInterpreter.interpreter.NumericValue.IntVal intCase)
                    resultStr = intCase.Item.ToString();
                else if (result is MathInterpreter.interpreter.NumericValue.FloatVal floatCase) 
                {
                    resultStr = floatCase.Item.ToString("0.0000");
                }
                else
                    resultStr = "Unknown result";

                AppendToConsole($"= {resultStr}", false);
                
                InputExpression = string.Empty;
                UpdateSymbolTable();
            }
            catch (MathInterpreter.Exceptions.LexerException e)
            {
                AppendExceptionToConsole(e);
            }
            catch (MathInterpreter.Exceptions.ParseException e)
            {
                AppendExceptionToConsole(e);
            }
            catch (MathInterpreter.Exceptions.DivisionByZeroException e)
            {
                AppendExceptionToConsole(e);
            }
            catch (MathInterpreter.Exceptions.FunctionArgsException e)
            {
                AppendExceptionToConsole(e);
            }
            
        }

        public void CompileExpression()
        {
            string expression = _inputExpression.ToString();
            if (expression == string.Empty) return;
            
            try
            {
                const string fileName = "user_code";
                string fileExtension = string.Empty;
                string code = string.Empty;
                
                if (SelectedLanguage == "C")
                {
                    code = MathInterpreter.interpreter.cCompile(expression);
                    CompilerOutput = code;
                    fileExtension = ".c";
                    
                    MathInterpreter.interpreter.writeToFile($"{fileName}{fileExtension}", code);
                    string path = AppContext.BaseDirectory;
                    string json = MathInterpreter.interpreter.gccCompile(path, 
                        $"{_compilerOutputDir}{fileName}{fileExtension}", 
                        $"{_compilerOutputDir}{fileName}");
                    JObject obj = JObject.Parse(json);

                    if (obj["type"]?.ToString() == "compile")
                    {
                        if (obj["exit"]?.ToString() == "0" && obj["out"] == null)
                        {
                            TerminalOutput = (string.IsNullOrWhiteSpace(obj["out"]?.ToString())
                                ? "GCC Compiled Successfully."
                                : obj["out"]?.ToString());
                        }
                        else
                        {
                            TerminalOutput = obj["err"]?.ToString() ?? string.Empty;
                        }
                    
                    }
                } else if (SelectedLanguage == "RISC-V")
                {
                    code = MathInterpreter.interpreter.riscvCompile(expression);
                    CompilerOutput = code;
                    TerminalOutput = string.Empty;
                    fileExtension = ".s";
                }
                
                InputExpression = string.Empty;
                UpdateSymbolTable();
            }
            catch (MathInterpreter.Exceptions.LexerException e)
            {
                AppendExceptionToConsole(e);
            }
            catch (MathInterpreter.Exceptions.ParseException e)
            {
                AppendExceptionToConsole(e);
            }
            catch (MathInterpreter.Exceptions.DivisionByZeroException e)
            {
                AppendExceptionToConsole(e);
            }
            catch (MathInterpreter.Exceptions.FunctionArgsException e)
            {
                AppendExceptionToConsole(e);
            }
        }

        public void UpdateSymbolTable()
        {
            SymbolTable.Clear();
            foreach (var symbolTableEntry in MathInterpreter.interpreter.symbTable)
            {
                SymbolTable.Add(new SymbolTableEntry{ SymbolTableKey = symbolTableEntry.Key, 
                                      SymbolTableValue = symbolTableEntry.Value.ToString()});
            }
        }
        
        public void AddSymbol(string key, string value)
        {
            SymbolTable.Add(new SymbolTableEntry() { SymbolTableKey = key, SymbolTableValue = value });
            
        }
    }
}

