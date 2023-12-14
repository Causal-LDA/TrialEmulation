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

# trial_msm works with analysis_weights = unweighted

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
          "value": ["(Intercept)", "assigned_treatment", "trial_period", "followup_time", "X1", "X2", "X3", "X4", "age_s"]
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

# trial_msm works with analysis_weights = p99

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
          "value": ["(Intercept)", "assigned_treatment", "trial_period", "followup_time", "X1", "X2", "X3", "X4", "age_s"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.46693136, -1.63471307, -0.39194898, -0.75152527, 1.31546169, 0.23796332, 0.54138231, 1.023308, -0.06542283]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.00800452, 1.26704723, 0.35471228, 0.37376728, 0.62056262, 0.5696091, 0.99111245, 0.80075479, 0.43668267]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-5.44262022, -4.11812564, -1.08718506, -1.48410914, 0.09915896, -0.87847052, -1.4011981, -0.54617138, -0.92132087]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-1.4912425, 0.8486995, 0.30328709, -0.0189414, 2.53176442, 1.35439716, 2.48396272, 2.59278739, 0.79047521]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.43940061, -1.29017532, -1.10497719, -2.010677, 2.11978881, 0.417766, 0.54623702, 1.27792929, -0.14981779]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.000583, 0.19698979, 0.26916947, 0.04435958, 0.03402386, 0.67611821, 0.58490302, 0.20127436, 0.88090837]
        }
      ]
    }

# trial_msm works with analysis_weights = weight_limits

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
          "value": ["(Intercept)", "assigned_treatment", "trial_period", "followup_time", "X1", "X2", "X3", "X4", "age_s"]
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

# trial_msm works with sample weights

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
          "value": ["(Intercept)", "assigned_treatment", "trial_period", "followup_time", "X1", "X2", "X3", "X4", "age_s"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.86140786, -1.51756024, -0.37353376, -0.82824879, 1.41958257, 0.14505724, 0.72382671, 0.86103731, 0.05905786]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.08163206, 1.19667548, 0.43416259, 0.36449798, 0.72582293, 0.56898797, 1.0903126, 0.881077, 0.40424615]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-5.9814067, -3.86304417, -1.22449244, -1.54266483, -0.00303037, -0.97015917, -1.41318598, -0.86587361, -0.7332646]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-1.74140902, 0.82792369, 0.47742493, -0.11383275, 2.8421955, 1.26027366, 2.8608394, 2.58794822, 0.85138033]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-3.5699828, -1.26814685, -0.86035454, -2.27230009, 1.95582493, 0.25493904, 0.66387081, 0.97725546, 0.14609382]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.000357, 0.20474551, 0.38959364, 0.02306839, 0.05048578, 0.79877019, 0.506773, 0.3284427, 0.88384733]
        }
      ]
    }

