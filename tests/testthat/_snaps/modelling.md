# Modelling works with where_case

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["names", "estimate", "robust_se", "2.5%", "97.5%", "z", "p_value"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["(Intercept)", "assigned_treatment", "factor(followup_time)1", "factor(followup_time)2", "factor(followup_time)3", "factor(followup_time)4", "X1", "X2", "X3", "X4", "age_s"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-5.7244369, -1.16602145, -0.06608159, -1.2440854, -15.48041975, -15.41173261, 2.12009182, -0.30716764, 0.55948724, 1.33660843, 0.08468846]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.30139124, 1.25619541, 0.54388987, 1.21696175, 0.78275918, 0.87168045, 0.865842, 0.44024115, 1.07397483, 0.97956078, 0.47265276]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-8.27516372, -3.62816445, -1.13210575, -3.62933043, -17.01462775, -17.1202263, 0.42304149, -1.17004031, -1.54550343, -0.58333071, -0.84171095]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.17371008, 1.29612155, 0.99994256, 1.14115963, -13.94621176, -13.70323892, 3.81714214, 0.55570502, 2.66447792, 3.25654756, 1.01108787]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-4.39870559, -0.92821662, -0.12149812, -1.022288, -19.7767336, -17.68048435, 2.44858971, -0.69772588, 0.52095005, 1.36449769, 0.1791769]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.00001089, 0.35329522, 0.90329651, 0.30664461, 0, 0, 0.01434167, 0.48534864, 0.60240157, 0.17241098, 0.8577988]
        }
      ]
    }

# data_modelling works with analysis_weights = unweighted

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["names", "estimate", "robust_se", "2.5%", "97.5%", "z", "p_value"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["(Intercept)", "assigned_treatment", "for_period", "followup_time", "X1", "X2", "X3", "X4", "age_s"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.06458286, -1.87374327, -0.4264429, -0.51804601, 1.25103652, 0.29782921, 0.19461737, 1.23038439, 0.00277732]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.9149175, 1.26128915, 0.35243196, 0.34753752, 0.62776641, 0.50601482, 0.91360903, 0.87869213, 0.42536068]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-4.85782116, -4.34587001, -1.11720954, -1.19921955, 0.02061436, -0.69395983, -1.59605632, -0.49185219, -0.83092962]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-1.27134456, 0.59838347, 0.26432373, 0.16312754, 2.48145868, 1.28961825, 1.98529106, 2.95262096, 0.83648426]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.3495729, -1.48557789, -1.21000067, -1.49061892, 1.99283763, 0.58857805, 0.21302041, 1.40024514, 0.00652933]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.00080936, 0.13739081, 0.22627863, 0.13606157, 0.04627923, 0.55614436, 0.83131104, 0.16143992, 0.99479039]
        }
      ]
    }

# data_modelling works with analysis_weights = p99

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["names", "estimate", "robust_se", "2.5%", "97.5%", "z", "p_value"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["(Intercept)", "assigned_treatment", "for_period", "followup_time", "X1", "X2", "X3", "X4", "age_s"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.36159222, -1.75190333, -0.41128025, -0.62884243, 1.36216732, 0.29427562, 0.42327973, 1.10685586, -0.04043048]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.98697165, 1.2796563, 0.34916838, 0.37558878, 0.61243124, 0.55254828, 0.96027879, 0.82266765, 0.44026014]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-5.29605666, -4.26002968, -1.09565028, -1.36499645, 0.16180208, -0.78871902, -1.45886669, -0.50557273, -0.90334035]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-1.42712779, 0.75622303, 0.27308978, 0.10731159, 2.56253256, 1.37727025, 2.30542615, 2.71928445, 0.82247939]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.40596634, -1.36904208, -1.17788514, -1.67428437, 2.22419632, 0.53257901, 0.44078838, 1.34544717, -0.09183317]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.0006593, 0.17098612, 0.2388424, 0.09407472, 0.02613524, 0.59432504, 0.65936621, 0.17848087, 0.92683059]
        }
      ]
    }

# data_modelling works with analysis_weights = weight_limits

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["names", "estimate", "robust_se", "2.5%", "97.5%", "z", "p_value"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["(Intercept)", "assigned_treatment", "for_period", "followup_time", "X1", "X2", "X3", "X4", "age_s"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.4667822, -1.63456472, -0.39192411, -0.75276356, 1.31501668, 0.23791974, 0.54116841, 1.0243666, -0.06489607]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.00805417, 1.26721153, 0.3545763, 0.37577944, 0.62039488, 0.56993745, 0.99056982, 0.79963855, 0.43664379]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-5.44256838, -4.11829932, -1.08689366, -1.48929126, 0.09904272, -0.87915767, -1.40034843, -0.54292496, -0.92071789]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-1.49099601, 0.84916988, 0.30304544, -0.01623586, 2.53099064, 1.35499715, 2.48268526, 2.59165816, 0.79092575]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.43908322, -1.28989098, -1.10533082, -2.00320581, 2.11964464, 0.41744887, 0.54632031, 1.28103704, -0.14862475]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.00058369, 0.19708851, 0.26901627, 0.0451552, 0.03403603, 0.67635012, 0.58484577, 0.20018066, 0.88184974]
        }
      ]
    }

# data_modelling works with sample weights

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["names", "estimate", "robust_se", "2.5%", "97.5%", "z", "p_value"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["(Intercept)", "assigned_treatment", "for_period", "followup_time", "X1", "X2", "X3", "X4", "age_s"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.54865874, -1.54017165, -0.4175523, -0.74069663, 1.31097919, 0.21127406, 0.51670844, 0.95202281, 0.03044352]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.2380501, 1.38676039, 0.38284172, 0.33401377, 0.66803671, 0.61024755, 1.22147932, 0.91635618, 0.40002335]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-5.97523694, -4.25822201, -1.16792207, -1.39536362, 0.00162724, -0.98481114, -1.87739102, -0.8440353, -0.75360224]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-1.12208053, 1.17787871, 0.33281747, -0.08602963, 2.62033115, 1.40735927, 2.91080791, 2.74808093, 0.81448928]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-2.86632885, -1.11062565, -1.09066561, -2.21756313, 1.96243585, 0.34621043, 0.42301858, 1.03892224, 0.07610435]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.00415263, 0.26672952, 0.27542005, 0.02658463, 0.04971176, 0.72918458, 0.67228171, 0.2988409, 0.93933608]
        }
      ]
    }

