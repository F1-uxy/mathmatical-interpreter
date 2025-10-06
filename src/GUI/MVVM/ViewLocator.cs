using System;
using System.Collections.Generic;
using System.Numerics;

namespace GUI.MVVM;

public class ViewLocator
{
    private readonly Dictionary<Type, Type> _maps = new()
    {
        {typeof(HelpViewModel), typeof(HelpWindow)},
        {typeof(AboutViewModel), typeof(AboutWindow)},
    };
    
    public Type GetViewType(Type viewModelType) =>
        _maps.TryGetValue(viewModelType, out var viewType) 
            ? viewType 
            : throw new InvalidOperationException($"View type {viewModelType} not mapped to window");
}