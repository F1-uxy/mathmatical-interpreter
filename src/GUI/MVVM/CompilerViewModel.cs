using System;
using GUI;
using GUI.MVVM;

namespace MathGUI.MVVM;

public class CompilerViewModel : ViewModelBase
{
    private readonly CompilerService _compilerService;
    private readonly MainViewModel _expressionService;
    private readonly ConsoleService _consoleService;
    private readonly SymbolTableService _symbolTableService;
    
    private string _selectedLanguage;

    public RelayCommand CompileEnter => new RelayCommand(_ => CompileExpression());
    public RelayCommand RunEnter => new RelayCommand(_ => RunBinary());
    
    public CompilerViewModel(MainViewModel expression, CompilerService compile, SymbolTableService symbols, ConsoleService console)
    {
        _compilerService = compile;
        _expressionService = expression;
        _consoleService = console;
        _symbolTableService = symbols;
        _selectedLanguage = "C";
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
    
    
    private void CompileExpression()
    {
        string expression = _expressionService.InputExpression.ToString();
        if (expression == string.Empty) return;

        try
        {
            CompilerResult result;
                
            if (SelectedLanguage == "C")
            {
                result = _compilerService.CompileC(expression);
            } else if (SelectedLanguage == "RISC-V")
            {
                result = _compilerService.CompileRiscV(expression);
            }
            else return;
                
            _expressionService.CompilerOutput = result.GeneratedCode;

            _expressionService.TerminalOutput = result.Success
                ? (string.IsNullOrWhiteSpace(result.StdErr)
                    ? (string.IsNullOrWhiteSpace(result.StdOut)
                        ? "Compiled successfully."
                        : result.StdOut)
                    : result.StdErr)
                : "Compilation failed.";

            _expressionService.InputExpression = string.Empty;
            _symbolTableService.UpdateSymbolTable(_expressionService.SymbolTable);
        }
        catch (Exception e)
        {
            _consoleService.AppendExceptionToConsole(e);
        }
    }

    private void RunBinary()
    {
        var result = _compilerService.RunBinary();
        
        _expressionService.TerminalOutput = result.Success
            ? (string.IsNullOrWhiteSpace(result.StdErr)
                ? (string.IsNullOrWhiteSpace(result.StdOut)
                    ? "Executed successfully."
                    : result.StdOut)
                : result.StdErr)
            : "Execution failed.";

        _expressionService.InputExpression = string.Empty;
        _symbolTableService.UpdateSymbolTable(_expressionService.SymbolTable);
    }
}