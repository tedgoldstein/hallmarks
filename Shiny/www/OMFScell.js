HOT=null;

function asv() {
    // debugger;
}


TypeMap = {
    "Neuroblastoma":"TARGET-NBL",
    "Breast Invasive Carcinoma":"TCGA-BRCA",
    "Acute Myeloid Leukemia":"TARGET-AML",
    "High-Risk Wilms Tumor":"TARGET-WT",
    "Glioblastoma Multiforme":"TCGA-GBM",
    "Ovarian Serous Cystadenocarcinoma":"TCGA-OV",
    "Lung Adenocarcinoma":"TCGA-LUAD",
    "Uterine Corpus Endometrial Carcinoma":"TCGA-UCEC",
    "Kidney Renal Clear Cell Carcinoma":"TCGA-KIRC",
    "Head and Neck Squamous Cell Carcinoma":"TCGA-HNSC",
    "Brain Lower Grade Glioma":"TCGA-LGG",
    "Thyroid Carcinoma":"TCGA-THCA",
    "Lung Squamous Cell Carcinoma":"TCGA-LUSC",
    "Prostate Adenocarcinoma":"TCGA-PRAD",
    "Skin Cutaneous Melanoma":"TCGA-SKCM",
    "Colon Adenocarcinoma":"TCGA-COAD",
    "Stomach Adenocarcinoma":"TCGA-STAD",
    "Bladder Urothelial Carcinoma":"TCGA-BLCA",
    "Osteosarcoma":"TARGET-OS",
    "Liver Hepatocellular Carcinoma":"TCGA-LIHC",
    "Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma":"TCGA-CESC",
    "Kidney Renal Papillary Cell Carcinoma":"TCGA-KIRP",
    "Sarcoma":"TCGA-SARC",
    "Acute Myeloid Leukemia":"TCGA-LAML",
    "Esophageal Carcinoma":"TCGA-ESCA",
    "Pancreatic Adenocarcinoma":"TCGA-PAAD",
    "Pheochromocytoma and Paraganglioma":"TCGA-PCPG",
    "Rectum Adenocarcinoma":"TCGA-READ",
    "Testicular Germ Cell Tumors":"TCGA-TGCT",
    "Thymoma":"TCGA-THYM",
    "Kidney Chromophobe":"TCGA-KICH",
    "Adrenocortical Carcinoma":"TCGA-ACC", "Mesothelioma":"TCGA-MESO",
    "Uveal Melanoma":"TCGA-UVM",
    "Rhabdoid Tumor":"TARGET-RT",
    "Lymphoid Neoplasm Diffuse Large B-cell Lymphoma":"TCGA-DLBC",
    "Uterine Carcinosarcoma":"TCGA-UCS",
    "Cholangiocarcinoma":"TCGA-CHOL",
    "Clear Cell Sarcoma of the Kidney":"TARGET-CCSK"
}

urlMap = {
    "PubMed": "https://www.ncbi.nlm.nih.gov/pubmed/",
    "ImmPort.Study.ID": "http://www.immport.org/immport-open/public/study/study/displayStudyDetail/",
    "Strain": "http://www.findmice.org/summary?query=",
    "Type": "https://portal.gdc.cancer.gov/projects/",
    "Experiment.ID": "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",
    "BioSample.ID": "https://trace.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?run=",
    "Repository.Accession": "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",
    "PI": "https://www.google.com/search?q="
}

/*
["show", "Type", "Subtype", "Species", "Study.Title", "PI",
"ImmPort.Study.ID", "PubMed", "Experiment.ID", "Cohort", "BioSample.ID",
"Repository.Accession", "Biosample.Name", "Biosample.Description",
"Strain", "Evading_growth_suppressors", "Evading_immune_destruction",
"Genome_instability", "Replicative_immortality",
"Reprogramming_energy_metabolism", "Resisting_cell_death",
"Sustained_angiogenesis", "Sustaining_proliferative_signaling",
"Tissue_invasion_and_metastasis", "Tumor.promoting_inflammation"]
*/

function urlWrap(colName, id) {
    if (colName == "Strain") {
        a = id.split(" ");
        id = a[a.length - 1];
    }

    /*
    if (colName == "Type" && id in TypeMap)
        id = TypeMap[id];
    */

    if (colName in urlMap) 
        return "<a href='" + urlMap[colName] + id + "'  target='OMFS-aux' >" + id + "</a>"
    else
        return id
}



OMFScell = function(instance, td, row, col, prop, value, cellProperties) {
    if (HOT == null) {
        HOT = instance
        HOT.addHook('afterRender', asv)
    }
    var pre  = '<div class="table-cell-wrapper"> <div class="table-cell">';
    var post = '</div> </div>'


    if (value == true || value == false) 
        Handsontable.CheckboxRenderer.apply(this, arguments)
    else {
        value = urlWrap(instance.getColHeader(col), value);
        value = pre + value + post;
        Handsontable.HtmlRenderer.apply(this, arguments)
    }


    /* slows it down
    if (instance.params && col < 2 ) {
        var ids = instance.params.BioSampleID;
        td.classList = 'all-sample-info sample-' + ids[row];
        if (document.ROWCOLORSHACK && ids[row] in document.ROWCOLORSHACK)
            td.style.backgroundColor = document.ROWCOLORSHACK[ ids[row] ]
    }
    */
}
