using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;

public class setValues : MonoBehaviour
{
    public TextMeshProUGUI time;
    public TextMeshProUGUI highScore;
    // Start is called before the first frame update
    private void OnEnable()
    {
        Cursor.lockState = CursorLockMode.None;
        time.text = playerTimer.instance.t.ToString();
        highScore.text = PlayerPrefs.GetFloat("Highscore", 0).ToString();
    }
    // Update is called once per frame
    void Update()
    {
        
    }
}
