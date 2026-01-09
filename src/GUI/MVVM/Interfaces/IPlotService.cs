using System.Collections.Generic;
using OxyPlot;
using OxyPlot.Series;

namespace MathGUI.MVVM;

public interface IPlotService
{
    List<LineSeries> CreateSeries(string expression, double xMin, double xMax, float step, bool markerEnabled);
}