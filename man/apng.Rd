\name{apng}
\alias{apng}
\title{Convert static pngs to animated png}
\usage{
apng(input_files = c(), output_file = "output.png",
num_plays = 0, delay_num = 0, delay_den = 0,
dispose_op = APNG_DISPOSE_OP_NONE,
blend_op = APNG_BLEND_OP_SOURCE)
}
\arguments{
\item{input_files}{to specify the names of the input files}
\item{output_file}{the name of the output file}
\item{num_plays}{the amount of times to repeat the animation (0 means forever)}
\item{delay_num}{the numerator of the frame delay (\eqn{delay = \frac{delay\_num}{delay\_den}}{delay = delay_num / delay_den})}
\item{delay_den}{the denominator of the frame delay (\eqn{delay = \frac{delay\_num}{delay\_den}}{delay = delay_num / delay_den})}
\item{dispose_op}{the frame disposal strategy \cr (APNG_DISPOSE_OP_NONE, APNG_DISPOSE_OP_BACKGROUND, \cr APNG_DISPOSE_OP_PREVIOUS)}
\item{blend_op}{the frame blending strategy \cr (APNG_BLEND_OP_SOURCE, APNG_BLEND_OP_OVER)}

For more information on blending and frame disposal strategies see \url{https://wiki.mozilla.org/APNG_Specification}.
}
\value{
Returns nothing, output is written to \emph{output_file}.
}
\description{
Combine multiple png files into an animated png file.
}
\examples{
input1 <- tempfile(pattern = "", fileext = ".png")
input2 <- tempfile(pattern = "", fileext = ".png")
output <- tempfile(pattern = "", fileext = ".png")

# Generate inputs.
png(filename=input1)
plot(1:40, (1:40)^2)
dev.off()
png(filename=input2)
plot(1:40, (-1*1:40)^3)
dev.off()

# Create an animated png.
apng(c(input1, input2), output)
}
