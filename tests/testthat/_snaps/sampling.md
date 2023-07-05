# do_sampling works as expected when case exist

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "trial_period", "followup_time", "outcome", "weight", "treatment", "catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [76, 87, 229, 342, 353, 150, 13, 413, 24, 237]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 0, 0, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 1, 1, 0, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 7, 1, 0, 2, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 7, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 1, 2, 0, 0, 0, 1, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 3, 0, 0, 0, 0, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [40, 52, 18, 61, 29, 97, 41, 52, 10, 100]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [44, 55, 62, 46, 55, 65, 52, 55, 72, 63]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 0, 1, 0, 0, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 10, 10, 10, 10, 10]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "trial_period", "followup_time", "outcome", "weight", "treatment", "catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [76, 87, 229, 342, 353]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5, 5, 5, 5, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 7, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 1, 2, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 3]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [40, 52, 18, 61, 29]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [44, 55, 62, 46, 55]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1]
        }
      ]
    }

# do_sampling works as expected when no cases exist

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "trial_period", "followup_time", "outcome", "weight", "treatment", "catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [54]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [7]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [7]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [7]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [49]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [10]
        }
      ]
    }

# sample_from_period works as expected

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "trial_period", "followup_time"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [179, 216, 261, 320, 264, 216, 69, 103, 170, 341, 136, 377, 497, 468, 80, 487, 18, 1, 34, 497, 487, 18, 339, 34, 468, 29, 105, 227, 226, 332, 80, 377, 69, 24, 427, 195, 369, 226, 103, 264, 103, 237, 466, 195, 237, 105, 321, 466, 103, 103, 377, 427, 158, 108, 237, 466]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [2, 3, 3, 4, 7, 7, 10, 10, 11, 12, 17, 19, 26, 28, 29, 31, 31, 32, 32, 34, 37, 37, 38, 40, 40, 41, 43, 45, 47, 52, 53, 56, 58, 62, 62, 65, 67, 71, 72, 75, 75, 76, 81, 84, 89, 91, 93, 103, 103, 104, 105, 106, 113, 116, 117, 120]
        }
      ]
    }

# sample_from_period works as expected with multiple proportions

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["sample_id", "id", "trial_period", "followup_time", "outcome", "weight", "treatment", "catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [216, 80, 430, 328, 261, 475, 66, 195, 76, 87, 229, 342, 353, 497, 459, 351, 409, 24, 34, 413, 427, 321, 334, 473, 80, 391, 427, 66, 64, 13]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 7, 7, 7, 8, 9, 12, 16, 18, 18, 19, 19, 24, 24, 25]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 2, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 2, 0, 0, 7, 0, 0, 1, 0, 2, 7, 0, 0, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [2, 1, 1, 1, 1, 7, 1, 1, 1, 1, 7, 1, 1, 7, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 2, 0, 0, 0, 0, 1, 0, 1, 2, 0, 0, 1, 0, 7, 0, 0, 1, 7, 2, 0, 0, 0, 0, 7, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 1, 1, 0, 0, 1, 0, 8, 0, 2, 2, 1, 2, 8, 0, 1, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [35, 58, 27, 72, 52, 40, 42, 47, 40, 52, 18, 61, 29, 119, 85, 146, 13, 10, 43, 52, 148, 29, 79, 106, 58, 92, 148, 42, 79, 41]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [80, 47, 35, 68, 61, 91, 61, 59, 44, 55, 62, 46, 55, 71, 58, 78, 48, 72, 76, 55, 53, 32, 65, 64, 47, 62, 53, 61, 81, 52]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [100, 100, 100, 1, 1, 100, 100, 100, 1, 1, 1, 1, 1, 100, 1, 100, 100, 100, 100, 1, 100, 100, 1, 1, 100, 1, 100, 100, 100, 1]
        }
      ]
    }

# case_control_sampling_trials works with separate_files = TRUE

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "trial_period", "followup_time", "outcome", "weight", "treatment", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [13, 148, 22, 108, 18, 18, 148, 127, 13, 87, 34, 87, 164, 13, 150, 12, 24, 12, 164, 148, 108, 12, 5, 101, 34, 103, 158, 101, 127, 144]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 3, 3, 6, 8, 9, 10, 12, 14, 16, 17, 26, 37, 38, 38, 39, 40, 52, 62, 63, 66, 81, 94, 95, 98, 104, 108, 118, 123]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [45, 32, 165, 21, 112, 112, 32, 19, 45, 43, 28, 43, 14, 45, 40, 21, 18, 21, 14, 32, 21, 21, 34, 46, 28, 62, 32, 46, 19, 64]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [51, 56, 41, 52, 74, 74, 56, 44, 51, 54, 75, 54, 56, 51, 64, 59, 71, 59, 56, 56, 52, 59, 64, 51, 75, 71, 63, 51, 44, 57]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [100, 100, 1, 100, 100, 100, 100, 100, 100, 100, 100, 1, 100, 1, 1, 100, 100, 100, 1, 100, 100, 1, 100, 100, 100, 100, 100, 100, 100, 1]
        }
      ]
    }

# case_control_sampling_trials works with separate_files = FALSE

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "trial_period", "followup_time", "outcome", "weight", "treatment", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [13, 148, 22, 108, 18, 18, 148, 127, 13, 87, 34, 87, 164, 13, 150, 12, 24, 12, 164, 148, 108, 12, 5, 101, 34, 103, 158, 101, 127, 144]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 3, 3, 6, 8, 9, 10, 12, 14, 16, 17, 26, 37, 38, 38, 39, 40, 52, 62, 63, 66, 81, 94, 95, 98, 104, 108, 118, 123]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [45, 32, 165, 21, 112, 112, 32, 19, 45, 43, 28, 43, 14, 45, 40, 21, 18, 21, 14, 32, 21, 21, 34, 46, 28, 62, 32, 46, 19, 64]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [51, 56, 41, 52, 74, 74, 56, 44, 51, 54, 75, 54, 56, 51, 64, 59, 71, 59, 56, 56, 52, 59, 64, 51, 75, 71, 63, 51, 44, 57]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [100, 100, 1, 100, 100, 100, 100, 100, 100, 100, 100, 1, 100, 1, 1, 100, 100, 100, 1, 100, 100, 1, 100, 100, 100, 100, 100, 100, 100, 1]
        }
      ]
    }

