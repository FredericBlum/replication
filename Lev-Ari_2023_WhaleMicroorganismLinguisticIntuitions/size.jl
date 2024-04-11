using Pkg
cd(Base.source_dir())        # Changes into the directory , where your "my_julia.jl" is.
Pkg.activate(".")            # active the project, with a  static environment
Pkg.update()                 # update the ..tom-files

using SQLite
using Tables
using DataFrames
using CairoMakie

# Stats
using Distributions
using Turing
using MCMCDiagnosticTools

ENV["COLUMNS"] = 20


stmt = """
SELECT
	l.cldf_name AS language,
	l.cldf_glottocode,
	l.family,
	p.cldf_name,
	f.cldf_segments
FROM
	formtable as f,
	languagetable as l,
	parametertable as p
WHERE
	f.cldf_parameterReference = p.cldf_id
	  AND
	f.cldf_languageReference = l.cldf_id
	  AND
	(f.cldf_parameterReference like "BIG"
		OR
	f.cldf_parameterReference like "SMALL"
		OR
	f.cldf_parameterReference like "LONG"
		OR
	f.cldf_parameterReference like "SHORT"); 
"""


lexibank = SQLite.DB("lexibank-analysed/lexibank.sqlite3")
SQLite.tables(lexibank)

parameter = DBInterface.execute(lexibank, "select * from FormTable;")
DataFrame(parameter)
data = DBInterface.execute(lexibank, stmt) |> DataFrame
data[!, "length"] = length.(split.(data[!, "cldf_segments"], " "))
data

let
	set_theme!(theme_dark())
	yticks = unique(data[!, "cldf_name"])
	colors = Makie.wong_colors()

	fig = Figure(size = (600, 400))
	ax = Axis(fig[1,1]; palette = (; patchcolor = colors), 
		yticks = (1:4, yticks))
	rainclouds!(data[!, "cldf_name"], data[!, "length"];
		xlabel = "Concept",
		ylabel = "Length", title = "My Title",
		plot_boxplots = true, 
		cloud_width=1.5, clouds=violin,
		orientation=:horizontal,
		# Boxplot
		boxplot_width=0.1,
		strokewidth=1,
		# Scatterplot
		markersize=5,
		jitter_width=0.05,
		color = colors[indexin(data[!, "cldf_name"], unique(data[!, "cldf_name"]))])
	fig
end

# Convert categorical to ID
data[!,:concept_id] = indexin(data.cldf_name, levels(data.cldf_name));

data[!, :Family] = coalesce.(data.Family, "Isolate")
data[!, :Family] = indexin(data.Family, levels(data.Family));
length(unique(data.Family))
@model function m1(fam, concept, length)
    # Set intercept prior.
    a ~ Normal(5, 2)

	# Predictor prior
	b ~ Normal(1, 1)

	σ_fam ~ filldist(Exponential(), 2)
    Rho ~ LKJ(2, 2)
    Σ = (σ_fam .* σ_fam') .* Rho
    ab ~ filldist(MvNormal([a,b], Σ), 192)
	a = ab[1,fam]
    b = ab[2,fam]

	μ = @. a + b * concept
    σ ~ Exponential()

    for i ∈ eachindex(length)
        length[i] ~ Normal(μ[i], σ)
    end
end


chain = sample(m1(data.Family, data.concept_id, data.length), NUTS(), 1000)
chain |> display
describe(chain)[1] |> display
df = DataFrame(chain);

first(df, 5) |> display

chain_eval = ess_rhat(chain)
# chain_eval = ess_rhat(chain)
chain_eval[Symbol(),:rhat]

