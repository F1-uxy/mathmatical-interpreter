using System.Windows;
using System.Windows.Controls;
using ICSharpCode.AvalonEdit;

namespace GUI
{
    public static class AvalonEditBinding
    {
        public static readonly DependencyProperty BoundTextProperty =
            DependencyProperty.RegisterAttached(
                "BoundText",
                typeof(string),
                typeof(AvalonEditBinding),
                new PropertyMetadata(default(string), OnBoundTextChanged));

        public static void SetBoundText(DependencyObject obj, string value)
            => obj.SetValue(BoundTextProperty, value);

        public static string GetBoundText(DependencyObject obj)
            => (string)obj.GetValue(BoundTextProperty);

        private static void OnBoundTextChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            if (d is TextEditor editor)
            {
                editor.Text = e.NewValue?.ToString() ?? "";
            }
        }
    }
}