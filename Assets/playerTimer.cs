using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;

public class playerTimer : MonoBehaviour
{
    public static playerTimer instance;
    public TextMeshProUGUI timer;
    public float t = 0;
    bool frozen = false;
    float highScore;
    public GameObject gameEndpanel;
    private void Awake()
    {
        instance = this;
    }
    // Start is called before the first frame update
    void Start()
    {
        highScore = PlayerPrefs.GetFloat("Highscore", 9999999);
    }
    public void gainTime()
    {
        t = Mathf.Max(0, t - 5f);
        timer.text = t.ToString();
    }
    // Update is called once per frame
    void Update()
    {
        if(!frozen)
        t += Time.deltaTime;
        timer.text = t.ToString();
    }
    public void endLevel()
    {
        frozen = true;
        if(t < highScore)
        {
            PlayerPrefs.SetFloat("Highscore", t);
        }
        gameEndpanel.SetActive(true);
       
    }
}
