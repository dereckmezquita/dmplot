#include <Rcpp.h>
#include <algorithm>
#include <vector>

using namespace Rcpp;

// Helper function to calculate conversion line and base line
std::vector<double> calculate_line(const NumericVector& high, const NumericVector& low, int period) {
    int n = high.size();
    std::vector<double> line(n);
    
    for (int i = period - 1; i < n; ++i) {
        double period_high = *std::max_element(&high[i - period + 1], &high[i + 1]);
        double period_low = *std::min_element(&low[i - period + 1], &low[i + 1]);
        line[i] = (period_high + period_low) / 2.0;
    }
    
    return line;
}

//' Ichimoku Cloud
//' Calculate the Ichimoku Cloud for a given price series
//' @param high The high prices
//' @param low The low prices
//' @param close The closing prices
//' @param tenkan_period The period for the Tenkan-sen (Conversion Line)
//' @param kijun_period The period for the Kijun-sen (Base Line)
//' @param senkou_period The period for the Senkou Span B (Leading Span B)
//' @return A list containing the Ichimoku Cloud components
Rcpp::List ichimoku_cloud(
    NumericVector high,
    NumericVector low,
    NumericVector close,
    int tenkan_period = 9,
    int kijun_period = 26,
    int senkou_period = 52
) {
    int n = close.size();
    
    // Calculate Tenkan-sen (Conversion Line)
    std::vector<double> tenkan_sen = calculate_line(high, low, tenkan_period);
    
    // Calculate Kijun-sen (Base Line)
    std::vector<double> kijun_sen = calculate_line(high, low, kijun_period);
    
    // Calculate Chikou Span (Lagging Span)
    std::vector<double> chikou_span(n);
    for (int i = 0; i < n - kijun_period; ++i) {
        chikou_span[i] = close[i + kijun_period];
    }
    
    // Calculate Senkou Span A (Leading Span A)
    std::vector<double> senkou_span_a(n + kijun_period);
    for (int i = 0; i < n; ++i) {
        senkou_span_a[i + kijun_period] = (tenkan_sen[i] + kijun_sen[i]) / 2.0;
    }
    
    // Calculate Senkou Span B (Leading Span B)
    std::vector<double> senkou_span_b = calculate_line(high, low, senkou_period);
    std::vector<double> senkou_span_b_future(n + kijun_period);
    for (int i = 0; i < n; ++i) {
        senkou_span_b_future[i + kijun_period] = senkou_span_b[i];
    }
    
    return List::create(
        _["tenkan_sen"] = tenkan_sen,
        _["kijun_sen"] = kijun_sen,
        _["chikou_span"] = chikou_span,
        _["senkou_span_a"] = senkou_span_a,
        _["senkou_span_b"] = senkou_span_b_future
    );
}


// -----

// Helper function to calculate the middle point of high and low over a period
std::vector<double> calculate_midpoint(const std::vector<double>& high, const std::vector<double>& low, int period) {
    std::vector<double> result(high.size(), NA_REAL);
    for (int i = period - 1; i < high.size(); ++i) {
        double period_high = *std::max_element(high.begin() + i - period + 1, high.begin() + i + 1);
        double period_low = *std::min_element(low.begin() + i - period + 1, low.begin() + i + 1);
        result[i] = (period_high + period_low) / 2.0;
    }
    return result;
}

// Helper function to shift a vector
std::vector<double> shift_vector(const std::vector<double>& vec, int shift, bool fill_na = true) {
    int n = vec.size();
    std::vector<double> result(n, NA_REAL);
    
    if (shift >= 0) {
        for (int i = shift; i < n; ++i) {
            result[i] = vec[i - shift];
        }
    } else {
        for (int i = 0; i < n + shift; ++i) {
            result[i] = vec[i - shift];
        }
        if (!fill_na) {
            for (int i = n + shift; i < n; ++i) {
                result[i] = vec[n - 1];  // Repeat last value
            }
        }
    }
    
    return result;
}

//' Ichimoku Cloud with Future Predictions
//'
//' @param high A numeric vector of high prices
//' @param low A numeric vector of low prices
//' @param close A numeric vector of closing prices
//' @param tenkan_period The period for Tenkan-sen (Conversion Line)
//' @param kijun_period The period for Kijun-sen (Base Line)
//' @param senkou_span_b_period The period for Senkou Span B
//' @param chikou_shift The shift for Chikou Span (typically 26)
//' @return A list containing Tenkan-sen, Kijun-sen, Senkou Span A, Senkou Span B, and Chikou Span
List ichimoku_cloud2(
    std::vector<double> high,
    std::vector<double> low,
    std::vector<double> close,
    int tenkan_period = 9,
    int kijun_period = 26,
    int senkou_span_b_period = 52,
    int chikou_shift = 26
) {
    int n = high.size();
    
    // Calculate Tenkan-sen (Conversion Line)
    std::vector<double> tenkan_sen = calculate_midpoint(high, low, tenkan_period);
    
    // Calculate Kijun-sen (Base Line)
    std::vector<double> kijun_sen = calculate_midpoint(high, low, kijun_period);
    
    // Calculate Senkou Span A (Leading Span A)
    std::vector<double> senkou_span_a(n, NA_REAL);
    for (int i = kijun_period - 1; i < n; ++i) {
        senkou_span_a[i] = (tenkan_sen[i] + kijun_sen[i]) / 2.0;
    }
    senkou_span_a = shift_vector(senkou_span_a, kijun_period, false);
    
    // Calculate Senkou Span B (Leading Span B)
    std::vector<double> senkou_span_b = calculate_midpoint(high, low, senkou_span_b_period);
    senkou_span_b = shift_vector(senkou_span_b, kijun_period, false);
    
    // Calculate Chikou Span (Lagging Span)
    std::vector<double> chikou_span = shift_vector(close, -chikou_shift);
    
    // Create and return the list
    List result = List::create(
        _["tenkan_sen"] = tenkan_sen,
        _["kijun_sen"] = kijun_sen,
        _["senkou_span_a"] = senkou_span_a,
        _["senkou_span_b"] = senkou_span_b,
        _["chikou_span"] = chikou_span
    );
    
    return result;
}
