using System;
using System.Windows;

namespace GUI.MVVM;

public class WindowService : IWindowService
{
    private readonly ViewLocator _viewLocator = new ViewLocator();
    public void ShowWindow<TViewModel>(TViewModel viewModel) where TViewModel : class
    {
        var viewType = _viewLocator.GetViewType(typeof(TViewModel));
        var window = (Window)Activator.CreateInstance(viewType)!;
        window.DataContext = viewModel;
        window.Show();
    }
}