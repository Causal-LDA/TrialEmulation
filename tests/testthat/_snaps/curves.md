# predict.RTE_model works as expected

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["assigned_treatment_0", "assigned_treatment_1", "difference"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.0046817, 0.00933138, 0.01394965, 0.01853709, 0.02309424, 0.02762164]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00358595, 0.00715416, 0.0107049, 0.01423843, 0.01775497, 0.02125478]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc_diff"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.00109575, -0.00217722, -0.00324475, -0.00429866, -0.00533927, -0.00636686]
            }
          ]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["assigned_treatment_0", "assigned_treatment_1", "difference"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.0046817, 0.00933138, 0.01394965, 0.01853709, 0.02309424, 0.02762164]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00358595, 0.00715416, 0.0107049, 0.01423843, 0.01775497, 0.02125478]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc_diff"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.00109575, -0.00217722, -0.00324475, -0.00429866, -0.00533927, -0.00636686]
            }
          ]
        }
      ]
    }

# predict.RTE_model works with newdata

    [
      [-6.0004, -0.4856, 0.004, 0.0007, -0.3353, -0.6904, -9.8769, 0.0369, -0.0519],
      [-5.9518, -0.3274, 0.0024, 0.0017, 0.0034, 0.0533, -10.3573, 0.0277, -0.0871],
      [-6.1427, 0.4223, 0.0028, 0.0013, 0.1963, -0.2092, -10.7104, 0.2131, -0.0384],
      [-5.9441, -0.0162, 0.003, 0.0012, -0.1526, -0.3053, -10.4805, 0.0507, -0.0624],
      [-6.1255, -0.2555, 0.0037, 0.0028, 0.0892, -0.3554, -11.1795, 0.3186, -0.1027]
    ]

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["assigned_treatment_0", "assigned_treatment_1", "difference"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc", "2.5%", "97.5%"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5, 6, 7, 8]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00543004, 0.01083993, 0.01622967, 0.0215993, 0.02694883, 0.03227829, 0.03758771, 0.0428771, 0.04814648]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00485021, 0.00968439, 0.01450255, 0.01930471, 0.02409088, 0.02886107, 0.03361531, 0.0383536, 0.04307597]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00666655, 0.01329289, 0.01987921, 0.02642575, 0.03293271, 0.0394003, 0.04582875, 0.05221825, 0.05856903]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc", "2.5%", "97.5%"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5, 6, 7, 8]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00498511, 0.00995399, 0.01490662, 0.01984303, 0.02476322, 0.02966719, 0.03455497, 0.03942656, 0.04428196]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00357991, 0.00715221, 0.01071691, 0.014274, 0.01782348, 0.02136535, 0.0248996, 0.02842624, 0.03194526]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00723324, 0.0144222, 0.02156709, 0.02866812, 0.03572549, 0.04273942, 0.0497101, 0.05663776, 0.06352259]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc_diff", "2.5%", "97.5%"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5, 6, 7, 8]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.00044493, -0.00088594, -0.00132305, -0.00175627, -0.00218561, -0.0026111, -0.00303273, -0.00345054, -0.00386452]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.00243937, -0.00485317, -0.00724156, -0.00960474, -0.01194289, -0.01425619, -0.01654481, -0.01880894, -0.02104875]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00223231, 0.00443933, 0.00662123, 0.00877818, 0.01091034, 0.01301788, 0.01510097, 0.01715978, 0.01919445]
            }
          ]
        }
      ]
    }

