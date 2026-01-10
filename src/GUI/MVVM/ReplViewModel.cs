using System;
using System.Collections.ObjectModel;
using System.Drawing.Printing;
using System.IO;
using System.Net;
using GUI;
using GUI.MVVM;
using Microsoft.Win32;

namespace MathGUI.MVVM;

public sealed class ReplViewModel : ViewModelBase
{
    private readonly ComputeService _computeService;
    private readonly SymbolTableService _symbolTableService;
    private readonly MainViewModel _expressionService;
    private readonly ConsoleService _consoleService;

    public ObservableCollection<SymbolTableEntry> SymbolTable { get; } = new();

    public RelayCommand InputEnter => new RelayCommand(_ => SubmitExpression());
    public RelayCommand OpenFileCommand => new RelayCommand(_ => LoadFile());

    public ReplViewModel(MainViewModel expression, ComputeService compute, SymbolTableService symbols, ConsoleService console)
    {
        _computeService = compute;
        _symbolTableService = symbols;
        _expressionService = expression;
        _consoleService = console;

        _consoleService.AppendToConsole(">> Welcome", false);
        
    }

    private void LoadFile()
    {
        var dialog = new OpenFileDialog
        {
            Filter = "Code files (*.ac)|*.ac|All files (*.*)|*.*",
            Title = "Select a text file"
        };

        if (dialog.ShowDialog() == true)
        {
            string filePath = dialog.FileName;
            string fileContents = File.ReadAllText(filePath);
            _expressionService.InputExpression = fileContents;
        }
    }
    
    private void SubmitExpression()
    {
        string expression = _expressionService.InputExpression.ToString();
        _consoleService.AppendToConsole(expression, true);
        
        if (expression == string.Empty) return;
        
        try
        {
            string resultStr = _computeService.EvaluateExpression(expression);
            _consoleService.AppendToConsole($"= {resultStr}", false);
            
            _expressionService.InputExpression = string.Empty;
            _symbolTableService.UpdateSymbolTable(SymbolTable);
        }
        catch (Exception e)
        {
            _consoleService.AppendExceptionToConsole(e);
        }
    }
    
}