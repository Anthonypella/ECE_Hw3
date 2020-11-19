using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;

public class health : MonoBehaviour
{
    public int maxHealth;
    public bool isEnemy = true;
    private int currentHealth;
    public float textSpawnHeight = 2f;
    public GameObject dmgText;
    private healthUi hBar;


    // Start is called before the first frame update
    void Start()
    {
        currentHealth = maxHealth;
        hBar = GetComponent<healthUi>();
       
        
    }

    public void takeDamage(int damage, bool damageEnemies)
    {
        Debug.Log(damage);
       
        if (damageEnemies == isEnemy)
        {
            if (damageEnemies == true)
            {
                GetComponent<enemy_Controller>().agro();
            }
            currentHealth -= damage;
            GameObject txt = Instantiate(dmgText, transform.position + Vector3.up * textSpawnHeight, Quaternion.identity);
            txt.GetComponent<TextMesh>().text = damage.ToString();
            hBar.updateSlider(currentHealth, maxHealth);
            if (currentHealth <= 0)
            {
                die();
            }
        }
        
    }
    public void slow(float slowAmt, float time)
    {
        GetComponent<enemy_Controller>().slowDown(slowAmt, time);
    }
    public void die()
    {
        if (!isEnemy)
        {
            GetComponent<playerController>().playerDeath();
        }
        playerTimer.instance.gainTime();
        Destroy(transform.gameObject);
    }
}
