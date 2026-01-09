using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using GUI;
using GUI.MVVM;
using OxyPlot;
using OxyPlot.Axes;
using OxyPlot.Series;

namespace MathGUI.MVVM;

public sealed class PlotViewModel : INotifyPropertyChanged
{
    private readonly PlotService _plotService;
    private ConsoleService _consoleService;
    private readonly MainViewModel _expressionService;

    public PlotModel Model { get; private set; }
    
    private string _currentExpression = string.Empty;
    private float _xMin = 0;
    private float _xMax = 10;
    private float _step = 0.1f;
    private bool _markerEnabled;
    private bool _scaleEnabled;
    
    public RelayCommand PlotEnter => new RelayCommand(_ => RedrawExpression());
    

    public PlotViewModel(MainViewModel expressionService, PlotService plotService, ConsoleService consoleService)
    {
        _plotService = plotService;
        _expressionService = expressionService;
        _consoleService = consoleService;
        
        Model = new PlotModel();

        Model.Axes.Add(new LinearAxis
        {
            Position = AxisPosition.Bottom,
            MajorGridlineStyle = LineStyle.Solid,
            MinorGridlineStyle = LineStyle.Dot
        });

        Model.Axes.Add(new LinearAxis
        {
            Position = AxisPosition.Left,
            MajorGridlineStyle = LineStyle.Solid,
            MinorGridlineStyle = LineStyle.Dot
        });

        Model.Axes[0].AxisChanged += OnAxisChanged;
        
        XMin = 0;
        XMax = 10;
        Step = 0.1f;
        CurrentExpression = "sin(x)";
        PlotExpression(CurrentExpression, XMin, XMax, Step);
    }

    public string CurrentExpression
    {
        get => _currentExpression;
        set => _currentExpression = value;
    }

    public float XMin
    {
        get => _xMin;
        set => _xMin = value;
    }

    public float XMax
    {
        get => _xMax;
        set => _xMax = value;
    }

    public float Step
    {
        get => _step;
        set => _step = value;
    }

    public bool MarkerEnabled
    {
        get => _markerEnabled;
        set => _markerEnabled = value;
    }

    public bool ScaleEnabled
    {
        get => _scaleEnabled;
        set => _scaleEnabled = value;
    }
    
    private void RedrawExpression()
    {
        PlotExpression(_expressionService.InputExpression, _xMin, _xMax, _step);
        Model.InvalidatePlot(false);
    }
    
    private void PlotExpression(string expression, double xMin, double xMax, float step)
    {
        if (expression == string.Empty) return;
        CurrentExpression = expression;
        try
        {
            var series = _plotService.CreateSeries(expression, xMin, xMax, step, MarkerEnabled);
            Model.Title = expression;
            Model.Series.Clear();
            Model.Series.Add(series);
        }
        catch (Exception e)
        {
            _consoleService.AppendExceptionToConsole($"Plot failed: {e.Message}");
        }
    }

    private void OnAxisChanged(object? sender, AxisChangedEventArgs e)
    {
        if (sender is LinearAxis xAxis && ScaleEnabled)
        {
            double min = xAxis.ActualMinimum;
            double max = xAxis.ActualMaximum;

            PlotExpression(CurrentExpression, min, max, _step);

            Model.InvalidatePlot(false);
        }
    }

    public event PropertyChangedEventHandler? PropertyChanged;

    private void OnPropertyChanged([CallerMemberName] string? propertyName = null)
    {
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    }
}
