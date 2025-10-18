using System;
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
        
        public RelayCommand ExitCommand => new RelayCommand(_ => Exit());
        
        public RelayCommand HelpWindowShow => new RelayCommand(_ => ShowHelpWindow());
        public RelayCommand AboutWindowShow => new RelayCommand(_ => ShowAboutWindow());

        public RelayCommand InputEnter => new RelayCommand(_ => SubmitExpression());


        public PlotModel MyModel { get; private set; }
        public MainViewModel()
        {
            //Func<double, double> myFun1 = (x) => 2 * x;
            Func<double, double> sinFunc = (x) => Math.Sin(x);
            this.MyModel = new PlotModel { Title = "Example 1" };
            //this.MyModel.Series.Add(new FunctionSeries(Math.Cos, 0, 10, 0.1, "cos(x)"));
            this.MyModel.Series.Add(new FunctionSeries(sinFunc, 0, 10, 0.1, "sin(x)"));

            StatusMessage = "Welcome";
        }
        
        
        public string StatusMessage
        {
            get => _statusMessage;
            set
            {
                if(_statusMessage != value)
                {
                    _statusMessage = value;
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

        private void SubmitExpression()
        {
            string expression = _inputExpression.ToString();
            if (expression == string.Empty) return;
            try
            {
                int result = MathInterpreter.interpreter.evaluate(expression);
                StatusMessage = result.ToString();
            }
            catch (MathInterpreter.Exceptions.LexerException e)
            {
                StatusMessage = e.Message;
            }
            catch (MathInterpreter.Exceptions.ParseException e)
            {
                StatusMessage = e.Message;
            }
            catch (MathInterpreter.Exceptions.DivisionByZeroException e)
            {
                StatusMessage = e.Message;
            }
            
        }
    }
}

