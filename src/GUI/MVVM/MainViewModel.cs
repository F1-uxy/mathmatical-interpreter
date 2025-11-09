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
using System.Runtime.CompilerServices;
using Microsoft.FSharp.Core;
using Microsoft.FSharp.Collections;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using MathInterpreter;
using GUI.MVVM;

namespace GUI
{
    public class MainViewModel : INotifyPropertyChanged
    {
        
        private readonly WindowService _windowService =  new WindowService();
        // Status TextBlock
        private string _statusMessage = string.Empty;
        private string _inputExpression = string.Empty;
        private float _xMinInput = 0;
        private float _xMaxInput = 0;
        private float _stepInput = 0;
        private bool _markerEnabled = false;
        
        public RelayCommand ExitCommand => new RelayCommand(_ => Exit());
        
        public RelayCommand HelpWindowShow => new RelayCommand(_ => ShowHelpWindow());
        public RelayCommand AboutWindowShow => new RelayCommand(_ => ShowAboutWindow());

        public RelayCommand InputEnter => new RelayCommand(_ => SubmitExpression());

        public RelayCommand PlotEnter => new RelayCommand(_ => PlotExpression());

        public PlotModel MyModel { get; private set; }


        public MainViewModel()
        {
            //Func<double, double> myFun1 = (x) => 2 * x;
            Func<double, double> sinFunc = (x) => Math.Sin(x);
            this.MyModel = new PlotModel { Title = "sin(x)" };
            this.MyModel.Series.Add(new FunctionSeries(sinFunc, 0, 10, 0.1, "sin(x)"));
            
            AppendToConsole(">> Welcome", false);
            XMinInput = 0;
            XMaxInput = 10;
            StepInput = 0.1f;
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

        public event PropertyChangedEventHandler? PropertyChanged;
        private void OnPropertyChanged([CallerMemberName] string? propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
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

        private void PlotExpression()
        {
            string expression = _inputExpression.ToString();
            if (expression == string.Empty) return;

            try
            {
                string json = MathInterpreter.interpreter.evalPlot(expression, XMinInput, XMaxInput, StepInput);
                JObject obj = JObject.Parse(json);
                string type = (string)obj["type"];

                if (type == "plot")
                {
                    double[] x = obj["x"].ToObject<double[]>();
                    double[] y = obj["y"].ToObject<double[]>();

                    PlotModel model = new PlotModel { Title = $"{expression}" };
                    LineSeries series = new LineSeries { Title = expression };
                    if (MarkerEnabled && series != null)
                    {
                        series.MarkerType = MarkerType.Circle;
                        series.MarkerSize = 3;
                        series.MarkerStroke = OxyColors.Black;
                    }

                    for (int i = 0; i < x.Length; i++)
                    {
                        series.Points.Add(new DataPoint(x[i], y[i]));
                    }

                    model.Series.Add(series);
                    MyModel = model;
                    OnPropertyChanged(nameof(MyModel));
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
                    resultStr = floatCase.Item.ToString();
                else
                    resultStr = "Unknown result";

                AppendToConsole($"= {resultStr}", false);
                InputExpression = string.Empty;
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
            
        }
    }
}

