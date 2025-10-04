namespace GUI.MVVM;

public interface IWindowService
{
    void ShowWindow<TViewModel>(TViewModel viewModel) where TViewModel : class;
}