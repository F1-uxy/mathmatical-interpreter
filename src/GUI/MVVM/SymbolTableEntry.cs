using System.ComponentModel;

namespace GUI
{
    public class SymbolTableEntry : INotifyPropertyChanged
    {
        private string _symbolTableKey;
        private string _symbolTableValue;

        public string SymbolTableKey
        {
            get => _symbolTableKey;
            set
            {
                if (_symbolTableKey != value)
                {
                    _symbolTableKey = value;
                    OnPropertyChanged(nameof(SymbolTableKey));
                }
            }
        }
        
        public string SymbolTableValue
        {
            get => _symbolTableValue;
            set
            {
                if (_symbolTableValue != value)
                {
                    _symbolTableValue = value;
                    OnPropertyChanged(nameof(SymbolTableValue));
                }
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;
        protected void OnPropertyChanged(string propertyName) =>
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    }
}