using System.Collections.ObjectModel;
using GUI;

namespace MathGUI.MVVM;

public class SymbolTableService : ISymbolTableService
{
    public void UpdateSymbolTable(ObservableCollection<SymbolTableEntry> symbolTable)
    {
        symbolTable.Clear();
        foreach (var symbolTableEntry in MathInterpreter.interpreter.symbTable)
        {
            symbolTable.Add(new SymbolTableEntry{ SymbolTableKey = symbolTableEntry.Key, 
                SymbolTableValue = symbolTableEntry.Value.ToString()});
        }
    }
    
    public void AddSymbol(ObservableCollection<SymbolTableEntry> symbolTable, string key, string value)
    {
        symbolTable.Add(new SymbolTableEntry() { SymbolTableKey = key, SymbolTableValue = value });
    }
}