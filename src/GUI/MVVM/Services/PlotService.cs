using System;
using System.Collections.Generic;
using System.Windows.Shapes;
using Newtonsoft.Json.Linq;
using OxyPlot;
using OxyPlot.Series;

namespace MathGUI.MVVM;

public class PlotService : IPlotService
{    
    public List<LineSeries> CreateSeries(string expression, double xMin, double xMax, float step, bool markerEnabled)
    {
        string json = MathInterpreter.interpreter.evalPlot(expression, xMin, xMax, step);
        JObject obj = JObject.Parse(json);
        string type = (string)obj["type"];
        var seriesList = new List<LineSeries>();

        if (type == "plotSegments")
        {
            int segmentIndex = 0;
            foreach (var segment in obj["segments"])
            {
                double[] x = segment["x"].ToObject<double[]>();
                double[] y = segment["y"].ToObject<double[]>();
                
                if (x.Length == 0) continue;
                
                var segmentSeries = new LineSeries
                {
                    Title = $"{expression} (segment {segmentIndex})",
                    LineStyle = LineStyle.Solid,
                    Color = OxyColors.Blue
                };

                if (markerEnabled)
                {
                    segmentSeries.MarkerType = MarkerType.Circle;
                    segmentSeries.MarkerSize = 3;
                    segmentSeries.MarkerStroke = OxyColors.Black;
                }

                for (int i = 0; i < x.Length; i++)
                {
                    segmentSeries.Points.Add(new DataPoint(x[i], y[i]));
                }

                seriesList.Add(segmentSeries);
                segmentIndex++;
            }
        }

        return seriesList;
    }
}