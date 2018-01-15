genes.all : genes.gmt
	cut -f3- genes.gmt | tr '\t' '\n' | sort | uniq | sed '/^$$/d' > $@


genes.gmt: 
	mkdir -p data/hallmarks.genes
	mkdir -p data/hallmarks
	sh -v go.filter
	sh coalesce.sh > genes.gmt

got.hallmarks : data/hallmarks/Evading_growth_suppressors data/hallmarks/Evading_immune_destruction data/hallmarks/Genome_instability data/hallmarks/Replicative_immortality data/hallmarks/Reprogramming_energy_metabolism data/hallmarks/Resisting_cell_death data/hallmarks/Sustained_angiogenesis data/hallmarks/Sustaining_proliferative_signaling data/hallmarks/Tissue_invasion_and_metastasis data/hallmarks/Tumor-promoting_inflammation
	touch got.hallmarks

data/hallmarks/Evading_growth_suppressors data/hallmarks/Evading_immune_destruction data/hallmarks/Genome_instability data/hallmarks/Replicative_immortality data/hallmarks/Reprogramming_energy_metabolism data/hallmarks/Resisting_cell_death data/hallmarks/Sustained_angiogenesis data/hallmarks/Sustaining_proliferative_signaling data/hallmarks/Tissue_invasion_and_metastasis data/hallmarks/Tumor-promoting_inflammation : go.hallmarks
	sh -v go.hallmarks
