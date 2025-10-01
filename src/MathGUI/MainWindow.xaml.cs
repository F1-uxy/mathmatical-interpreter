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
using MathGUI.MVVM;

namespace MathGUI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>

    public class MainViewModel : INotifyPropertyChanged
    {
        // Status TextBlock
        private string statusMessage = string.Empty;
        public RelayCommand ExitCommand => new RelayCommand(_ => Exit());

        public MainViewModel()
        {
            this.MyModel = new PlotModel { Title = "Example 1" };
            this.MyModel.Series.Add(new FunctionSeries(Math.Cos, 0, 10, 0.1, "cos(x)"));
            
            StatusMessage = "Welcome";

        }

        public PlotModel MyModel { get; private set; }

        public string StatusMessage
        {
            get => statusMessage;
            set
            {
                if(statusMessage != value)
                {
                    statusMessage = value;
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
    }

}