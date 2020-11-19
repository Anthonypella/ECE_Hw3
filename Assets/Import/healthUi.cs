using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class healthUi : MonoBehaviour
{
    public GameObject uiPrefab;
    public Transform target;

    Transform ui;
    Image healthSlider;
    Transform Cam;
    // Start is called before the first frame update
    void Start()
    {
        Cam = Camera.main.transform;
        foreach (Canvas c in GameObject.FindObjectsOfType<Canvas>())
        {
            
            if (c.renderMode == RenderMode.WorldSpace)
            {
               
                ui = Instantiate(uiPrefab, c.transform).transform;
             
                healthSlider = ui.GetChild(0).GetComponent<Image>();
                break;
            }
        }

    }

    // Update is called once per frame
    void LateUpdate()
    {
        if(ui != null)
        {
        ui.position = target.position;
        ui.forward = -Cam.forward;
        }
       
    }
   public void updateSlider(int current,int max)
    {
        if (current <= 0)
        {
            Destroy(ui.gameObject);
        }

        float ratio = (float)current / (float)max;

        if (healthSlider)
        {
            healthSlider.fillAmount = ratio;
        
        }
       


    }
}
