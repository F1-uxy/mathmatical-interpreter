using System.Collections.ObjectModel;
using GUI;

namespace MathGUI.MVVM;

public interface ISymbolTableService
{
    void UpdateSymbolTable(ObservableCollection<SymbolTableEntry> symbolTable);
    void AddSymbol(ObservableCollection<SymbolTableEntry> symbolTable, string key, string value);
}