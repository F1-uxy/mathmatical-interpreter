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
using MathGUI.MVVM;
using MathGUI.MVVM.Interfaces;
using OxyPlot.Axes;

namespace GUI
{
    public class MainViewModel : INotifyPropertyChanged, IExpressionService
    {
        public ReplViewModel Repl { get; }
        public PlotViewModel Plot { get; }
        public CompilerViewModel Compile { get; }
        
        private readonly WindowService _windowService =  new WindowService();
        
        // Status TextBlock
        private string _statusMessage = string.Empty;
        private string _inputExpression = string.Empty;
        private string _compilerOutput = string.Empty;
        private string _terminalOutput = string.Empty;
        
        private float _xMinInput = 0;
        private float _xMaxInput = 0;
        private float _stepInput = 0;
        private bool _markerEnabled = false;
        private bool _scaleEnabled = false;
        
        public RelayCommand ExitCommand => new RelayCommand(_ => Exit());
        
        public RelayCommand HelpWindowShow => new RelayCommand(_ => ShowHelpWindow());
        public RelayCommand AboutWindowShow => new RelayCommand(_ => ShowAboutWindow());

        public ObservableCollection<SymbolTableEntry> SymbolTable { get; } = new ObservableCollection<SymbolTableEntry>();
        public ObservableCollection<string> Languages { get; } = new() { "C", "RISC-V" };
        
        public MainViewModel()
        {
            var compute = new ComputeService();
            var plotter = new PlotService();
            var compiler = new CompilerService();
            var symbols = new SymbolTableService();
            var console = new ConsoleService(this);

            Repl = new ReplViewModel(this, compute, symbols, console);
            Plot = new PlotViewModel(this, plotter, console);
            Compile = new CompilerViewModel(this, compiler, symbols, console);

        }
        
        public string InputExpression
        {
            get => _inputExpression;
            set
            {
                if (_inputExpression == value) return;
                _inputExpression = value;
                OnPropertyChanged();
            }
        }
        
        public string StatusMessage
        {
            get => _statusMessage;
            set
            {
                if (_statusMessage == value) return;
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

    }
}

