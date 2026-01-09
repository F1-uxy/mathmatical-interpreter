using Newtonsoft.Json.Linq;
using OxyPlot;
using OxyPlot.Series;

namespace MathGUI.MVVM;

public class PlotService : IPlotService
{    
    public LineSeries CreateSeries(string expression, double xMin, double xMax, float step, bool markerEnabled)
    {
        string json = MathInterpreter.interpreter.evalPlot(expression, xMin, xMax, step);
        JObject obj = JObject.Parse(json);
        string type = (string)obj["type"];
        var series = new LineSeries { Title = expression };

        if (type == "plot")
        {
            double[] x = obj["x"].ToObject<double[]>();
            double[] y = obj["y"].ToObject<double[]>();

            if (markerEnabled)
            {
                series.MarkerType = MarkerType.Circle;
                series.MarkerSize = 3;
                series.MarkerStroke = OxyColors.Black;
            }

            for (int i = 0; i < x.Length; i++)
            {
                series.Points.Add(new DataPoint(x[i], y[i]));
            }

        }

        return series;
    }
}